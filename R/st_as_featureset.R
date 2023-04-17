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
  # TODO have better CRS handling. We prefer having _no_ crs over
  # a wrong one.
  if (is.na(sf::st_crs(x)) && is.na(sf::st_crs(crs))) {
    warning("CRS missing. Setting to EPSG:4326")
    crs <- 4326
  }
  crs_text <- validate_crs(crs)


  geoms <- featureset_geometry(x)

  res <- lapply(
    geoms[[1]],
    function(.x) c(list(attributes = c()), geometry = list(.x))
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
    warning("CRS missing. Setting to EPSG:4326")
    crs <- 4326
  }

  crs_text <- validate_crs(crs)

  geo <- sf::st_geometry(x)
  geom_list <- featureset_geometry(geo)
  x <- sf::st_drop_geometry(x)

  fields <- transpose(x)

  # if there are no attributes
  if (nrow(x) == 0) {
    rows <- lapply(
      geoms_list[[1]],
      function(.x) c(list(attributes = c()), geometry = list(.x))
    )
  } else {
    # if attributes extract the fields
    rows <- mapply(
      function(.x, .y) c(list(attributes = c(.y)), geometry = list(.x)),
      geom_list[[1]],
      fields,
      SIMPLIFY = FALSE
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

  rows <- lapply(fields, function(.x) list(attributes = .x))

  # cast to json
  jsonify::to_json(c(list(features = rows)), unbox = TRUE)

}


# featureset geometry helper ----------------------------------------------
#' Convert an object to featureset list structure
#'
#' The output of this is intended to be passed to `jsonify::to_json()`
#'
#' @param x an object of class `sfc` or `sf`
#' @keywords internal
featureset_geometry <- function(x) {
  # extract geometry
  x <- sf::st_geometry(x)

  # get class of geometry
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
