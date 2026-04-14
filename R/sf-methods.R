# st_crs method for feature layer
st_crs.FeatureLayer <- function(obj, ...) {
  spatialReference <- obj[["spatialReference"]] %||% list()
  arcgisutils::from_spatial_reference(spatialReference)
}

st_crs.ImageServer <- function(obj, ...) {
 if (rlang::has_name(obj, "extent")) {
    spatialReference <- obj[["extent"]][["spatialReference"]]
  } else {
    spatialReference <- obj[["spatialReference"]] %||% list()
  }
  arcgisutils::from_spatial_reference(spatialReference)
}

# Implement `st_transform()`
