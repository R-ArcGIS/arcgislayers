#' Open connection to remote resource
#'
#' Provided a URL, create an object referencing the remote resource.
#' Supports Feature Layers, Tables, Feature Servers, and Image Servers.
#'
#' @param url The url of the remote resource. Must be of length one.
#' @param token
#'
#' @export
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
    } else if ("layers" %in% names(meta)) {
      layer_class <- "FeatureServer"
    } else {
      stop("Cannot determine layer type")
    }
  }

  res <- switch(
    layer_class,
    "FeatureLayer" = structure(
      meta, class = layer_class, n = count_features(req, token), query = list()
    ),
    "Table" = structure(
      meta, class = layer_class, n = count_features(req, token), query = list()
    ),
    "FeatureServer" = structure(
      meta, class = layer_class
    ),
    "ImageServer" = structure(meta, class = layer_class)
  )

  res
}


