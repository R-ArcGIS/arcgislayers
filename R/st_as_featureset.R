# TODO address field type

#| question based on https://developers.arcgis.com/documentation/common-data-types/featureset-object.htm
#|
#| are field mappings required?
#| if so, are there require fields that need to be mapped? e.g. object id and global id?
#|



# sfc objects to json -----------------------------------------------------

#' Create Esri FeatureSet JSON
#'
#' Represent simple features as Esri JSON.
#'
#' @param x an sf or sfc class object
#' @param crs the coordinate reference system of the FeatureSet. Must be interpretable by `sf::st_crs()`
#' @param ... unused
#' @export
#' @rdname st_as_featureset
#' @examples
#' # create sfc object of points from multipoint
#' mpnt <- st_multipoint(
#'   matrix(runif(10, min = -180, max = 180), ncol = 2)
#' )
#'
#' pnt_sfc <- st_cast(st_sfc(mpnt), "POINT")
#'
#' # sfc method
#' st_as_featureset(pnt_sfc, crs = 4326)
#'
#' # sf method
#' pnt_sf <- st_sf(pnt_sfc)
#' pnt_sf[["id"]] <- 1:5
#'
#' st_as_featureset(pnt_sf, crs = 4326)
st_as_featureset <- function(x, ...) {
  UseMethod("st_as_featureset")
}



#' @export
#' @rdname st_as_featureset
st_as_featureset.sfc <- function(x, crs = 4326, ...) {

  # check CRS first
  if (is.na(sf::st_crs(x)) && is.na(sf::st_crs(crs))) {
    cli::cli_alert_warning("CRS missing. Setting to EPSG:4326")
    crs <- 4326
  }
  crs_text <- validate_crs(crs)


  geoms <- featureset_geometry(x)

  res <- purrr::map(
    geoms[[1]],
    ~c(list(attributes = c()), geometry = list(.x))
  )

  # cast to json
  jsonify::to_json(
    c(
      geometryType = names(geoms),
      crs_text,
      hasZ = has_z(x),
      hasM = has_m(x),
      list(features = res)
    ),
    unbox = TRUE
  )
}

# sf objects --------------------------------------------------------------
#' @export
#' @rdname st_as_featureset
st_as_featureset.sf <- function(x, crs = sf::st_crs(x), ...) {

  # check CRS first
  if (is.na(sf::st_crs(crs))) {
    cli::cli_alert_warning("CRS missing. Setting to EPSG:4326")
    crs <- 4326
  }

  crs_text <- validate_crs(crs)

  geo <- sf::st_geometry(x)
  geom_list <- featureset_geometry(geo)
  x <- sf::st_drop_geometry(x)

  fields <- purrr::transpose(x)

  if (length(x) == 0) {
    rows <- purrr::map(
      geom_list[[1]],
      ~c(list(attributes = c()), geometry = list(.x))
    )

  } else {

    rows <- purrr::map2(
      fields,
      geom_list[[1]],
      ~c(list(attributes = .x, geometry = .y))
    )

  }

  # cast to json
  jsonify::to_json(
    c(
      geometryType = names(geom_list),
      crs_text,
      hasZ = has_z(geo),
      hasM = has_m(geo),
      list(features = rows)
    ),
    unbox = TRUE
  )
}



# data.frame --------------------------------------------------------------
#' @export
#' @rdname st_as_featureset
st_as_featureset.data.frame <- function(x, ...) {
  fields <- purrr::transpose(x)

  rows <- purrr::map(fields, ~list(attributes = .x))

  # cast to json
  jsonify::to_json(
    c(
     #  geometryType = names(geom_list),
      # crs_text,
      #hasZ = has_z(geo),
      #hasM = has_m(geo),
      list(features = rows)
    ),
    unbox = TRUE
  )

}


# featureset geometry helper ----------------------------------------------


featureset_geometry <- function(x) {
  geom_type <- as.character(sf::st_geometry_type(x, by_geometry = FALSE))
  # identify geometry type
  esri_geo_type <- switch(
    geom_type,
    "POINT" = "esriGeometryPoint",
    "MULTIPOINT" = "esriGeometryMultipoint",
    "LINESTRING" = "esriGeometryPolyline",
    "MULTILINESTRING" = "esriGeometryPolyline",
    "POLYGON" = "esriGeometryPolygon",
    "MULTIPOLYGON" = "esriGeometryPolygon"
  )

  # error out if not one of the 6 types above
  if (is.null(esri_geo_type)) {
    cli::cli_abort("{.cls {geom_type}} is not a supported Esri geometry type")
  }

  # convert geometry

  geo_conversion_fn <- switch(
    geom_type,
    "POINT" = sfc_point_impl,
    "MULTIPOINT" = sfc_multipoint_impl,
    "LINESTRING" = sfc_linestring_impl,
    "MULTILINESTRING" = sfc_multilinestring_impl,
    "POLYGON" = sfc_polygon_impl,
    "MULTIPOLYGON" = sfc_multipolygon_impl
  )

  setNames(list(geo_conversion_fn(x)), esri_geo_type)

}
