#' Remove any null elements from a list
# from https://github.com/r-lib/httr2/blob/87011c0ff31019409c4a5a700b041f279f054361/R/compat-purrr.R
#' @keywords internal
compact <- function(.x) Filter(length, .x)

#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a


# Taken directly from purrr
transpose <- function(.l, .names = NULL) {
  transpose_impl(.l, .names)
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


#' Retrieve metadata for a feature layer
#'
#' @param request an httr2 request object. Should be the `base_req`
#'   object that is created from the provided feature layer url
#'
#' @keywords internal
fetch_layer_metadata <- function(request, token) {
  req_url <- httr2::req_url_query(
    request,
    f = "pjson",
    token = token
  )

  resp_string <- httr2::resp_body_string(
    httr2::req_perform(req_url)
  )

  meta <- RcppSimdJson::fparse(resp_string)

  detect_errors(meta)

  meta
}


#' Count the number of features in a feature layer
#'
#' @param request the base request created from the feature layer url.
#' @keywords internal
#' @returns a numeric with the total number of features in the feature layer
count_features <- function(request, token) {

  req_url <- httr2::req_url_query(
    httr2::req_url_path_append(request, "query"),
    returnCountOnly = "true",
    where = "1 = 1",
    f = "pjson",
    token = token
  )

  resp <- httr2::req_perform(req_url)

  RcppSimdJson::fparse(
    httr2::resp_body_string(resp),
    query = "/count"
  )
}


#' Cleary all query parameters
#'
#'
#'@keywords internal
clear_query <- function(x) {
  attr(x, "query") <- list()
  x
}

#' List fields in a a feature layer
#'
#'@keywords internal
list_fields <- function(x) {
  x[["fields"]]
}


#' Refresh layer
#'
#' Useful to update metadata after modifying a remote
#' @keywords internal
refresh_layer <- function(x) {
  query <- attr(x, "query")
  xurl <- x[["url"]]
  x <- switch(
    class(x)[1],
    FeatureLayer = feature_layer(xurl),
    Table = feature_table(xurl)
  )

  attr(x, "query") <- query
  x
}



#' Detect errors in parsed json response
#'
#' The requests responses from ArcGIS don't return the status code
#' in the response itself but rather from the body in the json.
#' So this function checks for the existence of the error field.
#' @keywords internal
detect_errors <- function(response) {
  if (!is.null(response[["error"]])) {

    err_msg <- strwrap(
      paste0("  Error ", response$error$messageCode, ": ", response$error$message),
      prefix = "    ",
      initial = ""
    )

    stop(
      c("Status code: ", response[["error"]][["code"]], "\n", paste0(err_msg, collapse = "\n")),
      call. = FALSE
    )

  }
}



