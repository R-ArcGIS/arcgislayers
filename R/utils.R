#' Remove any null elements from a list
# from https://github.com/r-lib/httr2/blob/87011c0ff31019409c4a5a700b041f279f054361/R/compat-purrr.R
#' @keywords internal
compact <- function(.x) Filter(length, .x)

#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a


# Taken directly from purrr
transpose <- function(.l, .names = NULL) {
  transpose_cpp(.l, .names)
}



# match a scalar character of predicate name to esri type
match_spatial_rel <- function(predicate) {
  # determine the spatial relationship (predicate)
  predicate <- tolower(predicate)
  esri_predicates <- c(
    "esriSpatialRelIntersects",
    "esriSpatialRelContains",
    "esriSpatialRelCrosses",
    # "esriSpatialRelEnvelopeIntersects",
    #   - don't provide this, just provide bbox and intersects
    # "esriSpatialRelIndexIntersects", idk what this is remove
    "esriSpatialRelOverlaps",
    "esriSpatialRelTouches",
    "esriSpatialRelWithin"
  )

  pred_arg_vals <- tolower(substr(esri_predicates, 15, nchar(esri_predicates)))
  # ensure a correct one has been chosen
  match.arg(predicate, pred_arg_vals)
  esri_predicates[grepl(predicate, esri_predicates, ignore.case = TRUE)]
}


#' Prepare json for spatial filters
#' @param x is a `FeatureLayer`
#' @param y is an `sf` object
#' @keywords internal
prepare_spatial_filter <- function(x, y, predicate) {

  layer_crs <- sf::st_crs(x)
  input_crs <- sf::st_crs(y)

  stopifnot(inherits(y, c("sfg", "sfc")))

  # extract sfg object from sfc
  if (inherits(y, "sfc")) {
    if (length(y) > 1){
      stop(
        "only able to filter one feature\n  consider unioning geometries with `sf::st_union()"
      )
    }
    y <- y[[1]]
  }

  if (inherits(y, "MULTIPOLYGON")) stop("`MULTIPOLYGON` not supported.")

  list(
    geometryType = determine_esri_geo_type(y),
    geometry = st_as_json(y, input_crs),
    # TODO i think this is inferred from geometry
    # inSR = validate_crs(input_crs)[["spatialReference"]]
    spatialRel = match_spatial_rel(predicate)
  )
}

