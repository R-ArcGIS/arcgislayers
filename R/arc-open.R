#' Open connection to remote resource
#'
#' Provided a URL, create an object referencing the remote resource.
#' The resultant object acts as a reference to the remote data source.
#'
#' To extract data from the remote resource utilize [`arc_select()`] for objects of class
#' `FeatureLayer` or `Table`. For `ImageServer`s, utilize [`arc_raster()`].
#'
#'  `r lifecycle::badge("experimental")`
#'
#' @param url The url of the remote resource. Must be of length one.
#' @param token your authorization token. By default checks the environment
#' variable `ARCGIS_TOKEN`. Set your token using `arcgisutils::set_auth_token()`.
#'
#' @seealso arc_select arc_raster
#' @export
#' @returns
#' Depending on the provided URL returns a `FeatureLayer`, `Table`, `FeatureServer`, or `ImageServer`.
arc_open <- function(url, token = Sys.getenv("ARCGIS_TOKEN")) {

  stopifnot("`url` must be of length 1" = length(url) == 1)

  # generate base request
  req <- httr2::request(url)

  # extract layer metadata
  meta <- compact(fetch_layer_metadata(req, token))
  meta[["url"]] <- url # set url for later use

  # layer class
  layer_class <- gsub("\\s", "", meta[["type"]])

  # if it's missing it means it's a server type. Need to deduce.
  if (length(layer_class) == 0) {
    if (any(grepl("pixel|band|raster", names(meta)))) {
      layer_class <- "ImageServer"
    } else if (grepl("MapServer", meta[["url"]])) {
      layer_class <- "MapServer"
    } else if ("layers" %in% names(meta) || grepl("FeatureServer", meta[["url"]])) {
      layer_class <- "FeatureServer"
    } else {
      stop("Cannot determine layer type")
    }
  }

  # construct the appropriate class based on the resultant `layer_class`
  res <- switch(
    layer_class,
    "FeatureLayer" =  structure(
      meta,
      class = layer_class,
      # if the layer does not have a query capability return NA for the attr
      n = ifelse(
        grepl("query", meta[["capabilities"]], TRUE),
        count_features(req, token),
        NA
      ),
      query = list()
    ),
    "Table" = structure(
      meta,
      class = layer_class,
      # if the layer does not have a query capability return NA for the attr
      n = ifelse(
        grepl("query", meta[["capabilities"]], TRUE),
        count_features(req, token),
        NA
      ),
      query = list()
    ),
    "FeatureServer" = structure(
      meta, class = layer_class
    ),
    "ImageServer" = structure(meta, class = layer_class),
    "MapServer" = structure(meta, class = layer_class)
  )

  res
}

