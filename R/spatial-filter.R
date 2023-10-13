#' Prepare feature geometry or bounding box for use as a spatial filter
#'
#' [prepare_spatial_filter()] converts a `sfc`, `sfg`, or `bbox` object to a
#' named list with ESRI JSON geometry for use as a spatial filter.
#'
#' @param filter_geom An object of class `bbox`, `sfc` or `sfg` used to filter
#'   query results based on a predicate function. If an `sfc` object is provided
#'   it will be transformed to the layers spatial reference. If the `sfc` is
#'   missing a CRS (or is an `sfg` object) it is assumed to use the same spatial
#'   reference as the FeatureLayer. If the `sfc` object has multiple features,
#'   the features are unioned with [sf::st_union()]. If an `sfc` object has
#'   MULTIPOLYGON geometry, the features are treated as polygonal coverage and
#'   unioned with `is_coverage = TRUE` before being cast to POLYGON geometry
#'   with [sf::st_cast()].
#' @inheritParams match_spatial_rel
#' @returns A named list with the geometryType, geometry (as ESRI JSON), and
#'   spatial relation predicate.
#' @keywords internal
prepare_spatial_filter <- function(filter_geom,
                                   crs,
                                   predicate,
                                   error_call = caller_env()) {
  check_inherits_any(
    filter_geom,
    class = c("sfc", "sfg", "bbox"),
    call = error_call
  )

  if (inherits(filter_geom, "sfg")) {
    filter_crs <- crs
  }

  if (inherits(filter_geom, "bbox")) {
    filter_geom <- sf::st_as_sfc(filter_geom)
  }

  # if its an sfc object it must be length one
  if (inherits(filter_geom, "sfc")) {
    if (length(filter_geom) > 1) {
      filter_geom <- sf::st_union(filter_geom)
    }

    # if the CRS is missing use the layer's CRS
    # otherwise use the CRS of the filter_geom object
    filter_crs <- sf::st_crs(filter_geom)

    if (is.na(filter_crs)) {
      filter_crs <- crs
    }

    # extract the sfg object which is used to write ESRI JSON
    filter_geom <- filter_geom[[1]]
  }

  # if a multi polygon stop, must be a single polygon see
  # related issue: https://github.com/R-ArcGIS/arcgislayers/issues/4
  if (inherits(filter_geom, "MULTIPOLYGON")) {
    cli::cli_inform(
      c(
        "!" = "{.arg filter_geom} can't have {.val MULTIPOLYGON} geometry.",
        "i" = "Using {.fn sf::st_union} and {.fn sf::st_cast} to create a
        coverage {.val POLYGON} for {.arg filter_geom}."
      ),
      call = error_call
    )

    filter_geom <- sf::st_union(filter_geom, is_coverage = TRUE)
    filter_geom <- sf::st_cast(filter_geom, to = "POLYGON")
  }

  list(
    geometryType = arcgisutils::determine_esri_geo_type(filter_geom),
    geometry = arcgisutils::as_esri_geometry(filter_geom, filter_crs),
    spatialRel = match_spatial_rel(predicate, error_call = error_call)
    # TODO: i think this is inferred from geometry
    # inSR = validate_crs(input_crs)[["spatialReference"]]
  )
}



#' Match a predicate name to a type of ESRI spatial relation
#'
#' [match_spatial_rel()] takes a scalar character vector with a predicate name
#' to a type of ESRI spatial relation.
#'
#' @returns One of the following spatial binary predicates:
#'
#' - esriSpatialRelIntersects
#' - esriSpatialRelContains
#' - esriSpatialRelCrosses
#' - esriSpatialRelOverlaps
#' - esriSpatialRelTouches
#' - esriSpatialRelWithin
#'
#' @param predicate Spatial predicate to use with `filter_geom`. Default
#'   `"intersects"`. Possible options are `"intersects"`, `"contains"`,
#'   `"crosses"`,  `"overlaps"`,  `"touches"`, and `"within"`.
#' @keywords internal
match_spatial_rel <- function(predicate, error_call = caller_env()) {
  # determine the spatial relationship (predicate)
  esri_predicates <- c(
    # Part of a feature from the query feature is contained in a feature from
    # target feature
    "esriSpatialRelIntersects",
    # Part or all of a feature from a query feature is contained within a
    # feature from target feature
    "esriSpatialRelContains",
    # feature from a query feature crosses a feature from target feature
    "esriSpatialRelCrosses",
    # envelope of the query feature intersects with the envelope of target
    # feature
    # "esriSpatialRelEnvelopeIntersects",
    # envelope of the query feature intersects the index entry for the target
    # feature
    # "esriSpatialRelIndexIntersects",
    # Features from the query feature overlap features in target feature
    "esriSpatialRelOverlaps",
    # feature from the query feature touches the border of a feature from target
    # feature
    "esriSpatialRelTouches",
    # feature from the query feature is completely enclosed by the feature from
    # target feature
    "esriSpatialRelWithin"
  )

  pred_arg_vals <- tolower(substr(esri_predicates, 15, nchar(esri_predicates)))

  # ensure a correct one has been chosen
  predicate <- tolower(predicate)
  predicate <- arg_match(predicate, pred_arg_vals, error_call = error_call)

  esri_predicates[grepl(predicate, esri_predicates, ignore.case = TRUE)]
}

