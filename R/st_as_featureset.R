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
#' 
#' @rdname st_as_json
st_as_featureset <- function(x, ...) {
  UseMethod("st_as_featureset")
}



#' 
st_as_featureset.sfc <- function(x, crs = st_crs(x), ...) {

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
#' 
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

  fields <- transpose(x)

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
#' 
st_as_featureset.data.frame <- function(x, ...) {
  fields <- transpose(x)

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
#' Convert an object to featureset list structure
#'
#' The output of this is intended to be passed to `jsonify::to_json()`
#'
#' @param x an object of class `sfc` or `sf`
#' @keywords internal
featureset_geometry <- function(x) {
  geom_type <- as.character(sf::st_geometry_type(x, by_geometry = FALSE))

  # identify geometry type
  # TODO this duplicates the above call..maybe this can be simplified
  esri_geo_type <- determine_esri_geo_type(x)

  # error out if not one of the 6 types above
  if (is.null(esri_geo_type)) {
    stop("`", geom_type, "` is not a supported Esri geometry type")
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


# For an object of class sfc determine the corresponding Esri
# geometry object type
determine_esri_geo_type <- function(geom) {
  geom_type <- as.character(sf::st_geometry_type(geom, by_geometry = FALSE))
  switch(
    geom_type,
    "POINT" = "esriGeometryPoint",
    "MULTIPOINT" = "esriGeometryMultipoint",
    "LINESTRING" = "esriGeometryPolyline",
    "MULTILINESTRING" = "esriGeometryPolyline",
    "POLYGON" = "esriGeometryPolygon",
    "MULTIPOLYGON" = "esriGeometryPolygon"
  )

}
