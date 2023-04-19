




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
#' Create Esri objects
#'
#' These functions convert R objects to Esri json representations. There are three
#' types of representations. These are, from smallest to largest, a [geometry object](https://developers.arcgis.com/documentation/common-data-types/geometry-objects.htm), a [feature](https://developers.arcgis.com/documentation/common-data-types/feature-object.htm) and a [feature set](https://developers.arcgis.com/documentation/common-data-types/featureset-object.htm).
#'
#' @details
#'
#' - `st_as_json()` converts an `sfg` object to a geometry object
#' - `st_as_features()` converts an `sfc` or `sf` object to a list of features
#' - `st_as_featureset()` converts an `sf`, `sfc`, or `data.frame` to a feature set object
#'
#' Geometry object contain the coordinates for a geometry. Features are geometries that
#' have associated attributes with them. This would be similar to a row in an sf object.
#' FeatureSets are a list of features that have additional metadata fields such as
#' `spatialReference`, `geomtryType`, and `fields`. FeatureSets correspond to an
#' `sf` object.
#'
#' Geometry objects are defined for 5 different types. These are:
#'
#'  - Point: `esriGeometryPoint`
#'  - Multipoint: `esriGeometryMultipoint`
#'  - Polyline: `esriGeometryPolyline`
#'    - note that polyline encompasses both LINESTRING and MULTILINESTRING
#'  - Polygon: `esriGeometryPolygon`
#'    - note that polygon encompasses both POLYGON and MULTIPOLYGON
#'  - Envelope: `esriGeometryEnvelope`
#'    - envelopes correspond with bounding boxes but can have a Z dimension
#'
#'
#' @param x an object of class `sfg`
#' @param crs a CRS ID, crs object, or a well-known text representation of CRS
#' @examples
#' library(sf)
#' st_as_json(st_point(c(0, 1, 3, 4)))
#' st_as_json(st_multipoint(x = matrix(runif(4), ncol = 2)))
#' st_as_json(st_linestring(x = matrix(runif(2), ncol = 2)))
#' st_as_json(st_linestring(x = matrix(runif(2), ncol = 2)))
#'
#' # polygon
#' m <- matrix(
#'   c(0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 2, 2, 1, 2, 3, 1, 3, 2, 0, 0, 0),
#'   ncol = 3,
#'   byrow = TRUE
#' )
#' st_as_json(st_polygon(list(m)))
#' @noRd
st_as_json <- function(x, crs, ...) {
  UseMethod("st_as_json")
}

#' @export
st_as_json.POINT <- function(x, crs = 4326, ...) {

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
st_as_json.MULTIPOINT <- function(x, crs = 4326, ...) {
  crs_text <- validate_crs(crs)
  geometry <- sfc_multipoint_impl(list(x))[[1]]

  res <- c(hasZ = has_z(x), hasM = has_m(x), geometry, crs_text)
  jsonify::to_json(res, unbox = TRUE)
}

#' @export
st_as_json.LINESTRING <- function(x, crs = 4326, ...) {
  crs_text <- validate_crs(crs)
  geometry <- sfc_linestring_impl(list(x))[[1]]

  res <- c(hasZ = has_z(x), hasM = has_m(x), geometry, crs_text)
  jsonify::to_json(res, unbox = TRUE)
}

#' @export
st_as_json.MULTILINESTRING <- function(x, crs = 4326, ...) {
  crs_text <- validate_crs(crs)
  geometry <- sfc_multilinestring_impl(list(x))[[1]]

  res <- c(hasZ = has_z(x), hasM = has_m(x), geometry, crs_text)
  jsonify::to_json(res, unbox = TRUE)
}

#' @export
st_as_json.POLYGON <- function(x, crs = 4326, ...) {
  crs_text <- validate_crs(crs)
  geometry <- sfg_polygon_impl(x)
  res <- c(hasZ = has_z(x), hasM = has_m(x), geometry, crs_text)
  jsonify::to_json(res, unbox = TRUE)
}

#' @export
st_as_json.MULTIPOLYGON <- function(x, crs = 4326, ...) {
  crs_text <- validate_crs(crs)
  geometry <- sfc_multipolygon_impl(list(x))[[1]]
  res <- c(hasZ = has_z(x), hasM = has_m(x), geometry, crs_text)
  jsonify::to_json(res, unbox = TRUE)
}



#' @export
st_as_json.envelope <- function(x, crs = sf::st_crs(x)) {
  crs_text <- validate_crs(crs)
  jsonify::to_json(c(as.list(x), crs_text), unbox = TRUE)

}



# utils -------------------------------------------------------------------

# helpers to check for z or m dimension
has_m <- function(x) UseMethod("has_m")
#' @export
has_m.sfc <- function(x) !is.null(attr(x, "m_range"))
#' @export
has_m.sfg <- function(x) grepl("M", class(x)[1])

has_z <- function(x) UseMethod("has_z")
#' @export
has_z.sfc <- function(x) !is.null(attr(x, "z_range"))
#' @export
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
    stop(
      "Invalid `crs` argument supplied must be a \n  ",
      "- CRS ID,\n  ",
      "- well-known text representation,\n  ",
      "- or a `crs` object see `sf::st_crs()`"
    )
  }

  crs <- sf::st_crs(crs)

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


