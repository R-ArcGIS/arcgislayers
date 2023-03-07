# url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/List_of_Providers/FeatureServer"
#
# x <- feature_server(url)
# x
#
#
#
# #https://developers.arcgis.com/python/api-reference/arcgis.raster.toc.html#imagerylayer
#
# rr <- terra::rast("/vsicurl/https://sentinel-cogs.s3.us-west-2.amazonaws.com/sentinel-s2-l2a-cogs/43/M/BP/2021/6/S2A_43MBP_20210622_0_L2A/B08.tif")


#
#
# # https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer
#
# url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"
#
#
#' Image Server Representation
#' @export
image_server <- function(url, token = Sys.getenv("ARCGIS_TOKEN")) {
  req <- httr2::request(url)
  meta <- fetch_layer_metadata(req, token = token)
  meta <- compact(meta)
  meta[["url"]] <- url

  structure(
    meta,
    class = "ImageServer"
  )
}
#
#
# x <- image_server(url)
#
#' Print Method
#' @export
print.ImageServer <- function(x, ...) {

  header <- sprintf(
    "<%s <%i bands, %i fields>>",
    class(x), x$bandCount, length(x$fields$name) %||% 0
  )

  extent <- paste(
    round(x[["extent"]][["xmin"]], 2),
    round(x[["extent"]][["xmax"]], 2),
    round(x[["extent"]][["ymin"]], 2),
    round(x[["extent"]][["ymax"]], 2),
    "(xmin, xmax, ymin, ymax)"
    )

  to_print <- compact(list(
    "Name" = x[["name"]],
    "Description" = substr(x[["description"]], 1, options('width')$width %||% 80 - 14),
    "Extent" = extent,
    "Resolution" = paste(round(x$pixelSizeX, 2), "x", round(x$pixelSizeY, 2)),
    "CRS" = x[["extent"]][["spatialReference"]][["latestWkid"]],
    "Capabilities" = x[["capabilities"]]
  ))

  body <- paste0(names(to_print), ": ", to_print)
  # cat out
  cat(header, body, sep = "\n")
  invisible(x)

}
#
# x
# # extent
# # crs
#
# # name
# # description
# # dimensions
# # no. bands (names)
#
# x$pixelSizeX
# x$pixelSizeY
#
# # https://desktop.arcgis.com/en/arcmap/latest/manage-data/raster-and-images/bit-depth-capacity-for-raster-dataset-cells.htm
# x$pixelType
