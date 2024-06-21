# st_crs method for feature layer
st_crs.FeatureLayer <- function(obj, ...) {
  spatialReference <- obj[["extent"]][["spatialReference"]]

  obj_crs <- spatialReference[["latestWkid"]] %||%
    spatialReference[["wkid"]] %||%
    spatialReference[["wkt"]]

  sf::st_crs(obj_crs)
}

st_crs.ImageServer <- function(obj, ...) {
  spatialReference <- obj[["extent"]][["spatialReference"]]

  obj_crs <- spatialReference[["latestWkid"]] %||%
    spatialReference[["wkid"]] %||%
    spatialReference[["wkt"]]

  sf::st_crs(obj_crs)
}


# Implement `st_transform()`
