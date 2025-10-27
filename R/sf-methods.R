# st_crs method for feature layer
st_crs.FeatureLayer <- function(obj, ...) {
  arc_sr_as_crs(obj)
}

st_crs.ImageServer <- function(obj, ...) {
  arc_sr_as_crs(obj)
}

#' Get a Coordinate Reference System from a FeatureLayer or ImageServer spatial
#' reference
#'
#' arc_sr_as_crs() converts a FeatureLayer or ImageServer into a coordinate
#' reference system (CRS) using sf::st_crs().
#'
#' @param x A FeatureLayer or ImageServer or another object with a named
#'   `"spatialReference"` element that can be converted to a coordinate
#'   reference system with [sf::st_crs()].
#' @seealso [arcgisutils::validate_crs()]
#' @returns An object of class crs of length 2.
#' @noRd
arc_sr_as_crs <- function(x) {
  if (rlang::has_name(x, "extent")) {
    spatialReference <- x[["extent"]][["spatialReference"]]
  } else {
    spatialReference <- x[["spatialReference"]] %||% list()
  }

  x_crs <- spatialReference[["latestWkid"]] %||%
    spatialReference[["wkid"]] %||%
    spatialReference[["wkt"]]

  sf::st_crs(x_crs)
}

# Implement `st_transform()`
