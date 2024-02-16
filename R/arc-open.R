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
#' @param token your authorization token.
#'
#' @seealso arc_select arc_raster
#' @export
#' @returns
#' Depending on the provided URL returns a `FeatureLayer`, `Table`, `FeatureServer`, `ImageServer`, or `MapServer`. Each of these objects is a named list containing the properties of the service.
#' @examples
#'
#'if (interactive()) {
#'  # FeatureLayer
#'  furl <- paste0(
#'    "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/",
#'    "PLACES_LocalData_for_BetterHealth/FeatureServer/0"
#'  )
#'
#'  arc_open(furl)
#'
#'  # Table
#'  furl <- paste0(
#'    "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/",
#'    "USA_Wetlands/FeatureServer/1"
#'  )
#'
#'  arc_open(furl)
#'
#'  # ImageServer
#'  arc_open(
#'    "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"
#'    )
#'
#'  # FeatureServer
#'  furl <- paste0(
#'    "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/",
#'    "PLACES_LocalData_for_BetterHealth/FeatureServer"
#'  )
#'
#'  arc_open(furl)
#'
#'  # MapServer
#'  map_url <- paste0(
#'    "https://services.arcgisonline.com/ArcGIS/rest/services/",
#'    "World_Imagery/MapServer"
#'  )
#'
#'  arc_open(map_url)
#'
#'}
arc_open <- function(url, token = arc_token()) {

  stopifnot("`url` must be of length 1" = length(url) == 1)

  # extract layer metadata
  meta <- compact(fetch_layer_metadata(url, token))

  # set url for later use
  meta[["url"]] <- url

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
      query = list()
    ),
    "Table" = structure(
      meta,
      class = layer_class,
      query = list()
    ),
    "FeatureServer" = structure(
      meta, class = layer_class
    ),
    "ImageServer" = structure(meta, class = layer_class),
    "MapServer" = structure(meta, class = layer_class),
    "GroupLayer" = structure(meta, class = layer_class),
    cli::cli_abort(
      c(
        "Unsupported service type",
        "i"=  "Please report this at {.url https://github.com/R-ArcGIS/arcgislayers/issues}"
      )
    )
  )

  res
}

