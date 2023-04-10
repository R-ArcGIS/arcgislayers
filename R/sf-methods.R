
# st_crs method for feature layer
st_crs.FeatureLayer <- function(obj, ...) {
  sf::st_crs(obj[["extent"]][["spatialReference"]][["latestWkid"]])
}

st_crs.ImageServer <- function(obj, ...) {
  sf::st_crs(obj[["extent"]][["spatialReference"]][["latestWkid"]])
}


# Implement `st_transform()`
