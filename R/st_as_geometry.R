library(sf)
library(jsonify)

# TODO add hasZ and hasM attributes

# these functions cast an sfc geometry object into a list that, when passed into
# jsonify::to_json() will return the appropriate json structure for esri Geometry




#| Notes -------------------------------------------------------------------
#|
#| There are 3 representations of data that we need to concern ourselves with.
#| 1. geometry objects
#| 2. features
#| 3. feature sets
#|
#| Geometry objects are broken down into 5 types:
#|  - Point: `esriGeometryPoint`
#|  - Multipoint: `esriGeometryMultipoint`
#|  - Polyline: `esriGeometryPolyline`
#|    - note that polyline encompasses both LINESTRING and MULTILINESTRING
#|  - Polygon: `esriGeometryPolygon`
#|    - note that polygon encompasses both POLYGON and MULTIPOLYGON
#|  - Envelope: `esriGeometryEnvelope`
#|
#| Every geometry object is required to have the geometry and a `spatialReference`.
#| The spatial reference can have either `wkid` which is the well-known ID of the
#| spatial reference e.g 4326 or wkt.
#| Esri spatial references can be found at https://spatialreference.org/ref/esri/
#|
#| 3D geometries are also possible for point, multipoint, polygon, and envelope.
#| 3D geometries have a Z field and an optional M field (for measurement).
#| 3D geometries have top level fields `hasZ` and `hasM` which are used to
#| identify if the provided geometry is 3D.


# sfg object conversion ---------------------------------------------------
#' Convert an sf object to esri geometry
#'
#' @export
#' @param x an object of class `sfg`
#' @param crs a CRS ID, crs object, or a well-known text representation of CRS
#' @examples
#' library(sf)
#' st_as_geometry(st_point(c(0, 1, 3, 4)))
#' st_as_geometry(st_multipoint(x = matrix(runif(4), ncol = 2)))
#' st_as_geometry(st_linestring(x = matrix(runif(2), ncol = 2)))
#' st_as_geometry(st_linestring(x = matrix(runif(2), ncol = 2)))
#'
#' # polygon
#' m <- matrix(
#'   c(0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 2, 2, 1, 2, 3, 1, 3, 2, 0, 0, 0),
#'   ncol = 3,
#'   byrow = TRUE
#' )
#' st_as_geometry(st_polygon(list(m)))
st_as_geometry <- function(x, crs, ...) {
  UseMethod("st_as_geometry")
}

#' @export
#' @rdname st_as_geometry
st_as_geometry.POINT <- function(x, crs = 4326, ...) {

  crs_text <- validate_crs(crs)

  dims <- determine_dims(x)

  geometry <- switch(
    dims,
    "xy" = sfc_point_xy(list(x))[[1]],
    "xyz" = sfc_point_xyz(list(x))[[1]],
    "xyzm" = sfc_point_xyzm(list(x))[[1]],
  )

  res <- c(hasZ = has_z(x), hasM = has_m(x), geometry, crs_text)

  jsonify::to_json(res, unbox = TRUE)
}

#' @export
#' @rdname st_as_geometry
st_as_geometry.MULTIPOINT <- function(x, crs = 4326, ...) {
  crs_text <- validate_crs(crs)
  geometry <- sfc_multipoint_impl(list(x))[[1]]

  res <- c(hasZ = has_z(x), hasM = has_m(x), geometry, crs_text)
  jsonify::to_json(res, unbox = TRUE)
}

#' @export
#' @rdname st_as_geometry
st_as_geometry.LINESTRING <- function(x, crs = 4326, ...) {
  crs_text <- validate_crs(crs)
  geometry <- sfc_linestring_impl(list(x))[[1]]

  res <- c(hasZ = has_z(x), hasM = has_m(x), geometry, crs_text)
  jsonify::to_json(res, unbox = TRUE)
}

#' @export
#' @rdname st_as_geometry
st_as_geometry.MULTILINESTRING <- function(x, crs = 4326, ...) {
  crs_text <- validate_crs(crs)
  geometry <- sfc_multilinestring_impl(list(x))[[1]]

  res <- c(hasZ = has_z(x), hasM = has_m(x), geometry, crs_text)
  jsonify::to_json(res, unbox = TRUE)
}

#' @export
#' @rdname st_as_geometry
st_as_geometry.POLYGON <- function(x, crs = 4326, ...) {
  crs_text <- validate_crs(crs)
  geometry <- sfg_polygon_impl(x)
  res <- c(hasZ = has_z(x), hasM = has_m(x), geometry, crs_text)
  jsonify::to_json(res, unbox = TRUE)
}

#' @export
#' @rdname st_as_geometry
st_as_geometry.MULTIPOLYGON <- function(x, crs = 4326, ...) {
  crs_text <- validate_crs(crs)
  geometry <- sfc_multipolygon_impl(list(x))[[1]]
  res <- c(hasZ = has_z(x), hasM = has_m(x), geometry, crs_text)
  jsonify::to_json(res, unbox = TRUE)
}



#' @export
#' @rdname st_as_geometry
st_as_geometry.envelope <- function(x, crs = sf::st_crs(x)) {
  crs_text <- validate_crs(crs)
  jsonify::to_json(c(as.list(x), crs_text), unbox = TRUE)

}



# utils -------------------------------------------------------------------

# helpers to check for z or m dimension
has_m <- function(x) UseMethod("has_m")
has_m.sfc <- function(x) !is.null(attr(x, "m_range"))
has_m.sfg <- function(x) grepl("M", class(x)[1])

has_z <- function(x) UseMethod("has_z")
has_z.sfc <- function(x) !is.null(attr(x, "z_range"))
has_z.sfg <- function(x) grepl("Z", class(x)[1])

# helpers to identify xy, xyz, or xyzm dimensions

#' Determine the dimensions of a geometry object
#'
#' Given an sfc or sfg object determine what dimensions are represented.
#'
#' @returns a scalar character of the value `"xy"`, `"xyz"`, or `"xyzm"` depending
#' on what dimensions are represented.
#'
#' @keywords internal
determine_dims <- function(x) UseMethod("determine_dims")

#' @keywords internal
#' @rdname determine_dims
determine_dims.sfc <- function(x) {
  c("xyzm", "xyz", "xy")[c(has_m(x), has_z(x), TRUE)][1]
}

#' @keywords internal
#' @rdname determine_dims
determine_dims.sfg <- function(x) {
  tolower(class(x)[1])
}

# can be text representation, numeric, or crs object
# will be validated by sf and thus gdal
#' @keywords internal
validate_crs <- function(crs) {

  if (!(inherits(crs, "character") || inherits(crs, "crs") || inherits(crs, "numeric"))) {
    cli::cli_abort(c(
      "Invalid {.arg crs} argument supplied",
      "i" = "must be a CRS ID, well-known text representation,
        or a {.cls crs} object",
      "*" = "see `sf::st_crs()`"
    ))
  }

  crs <- sf::st_crs(crs)

  if (is.na(crs)) cli::cli_alert_warning("{.var crs} is not set")
  srid <- crs$srid

  if (!is.null(srid)) {
    wkid <- as.integer(strsplit(srid, ":")[[1]][2])
    wkt = NULL
  } else {
    wkt = crs$wkt
  }


  sr_components <- compact(list(wkid = wkid, wkt = wkt))
  list(spatialReference = sr_components)
}


