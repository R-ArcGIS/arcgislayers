#' Retrieve Imagery
#'
#' @param x an `ImageServer` as created with `image_server()`
#' @param bbox an object of class `bbox` from the sf package. Determines the extent of the raster returned. If the `bbox` object has a CRS, it will be used as the `bboxSR` parameter, otherwise the CRS of `x` is used.
#' @param format default `"tiff"`. Must be one of "jpgpng", "png", "png8", "png24", "jpg", "bmp", "gif", "tiff", "png32", "bip", "bsq", "lerc".
#' @param width default `NULL`. Cannot exceed `x[["maxImageWidth"]]`.
#' @param height default `NULL`. Cannot exceed `x[["maxImageHeight"]]`.
#' @param token authorization token fetched from the environment variable `ARCGIS_TOKEN`
#'
#' @examples
#' if (interactive()) {
#' img_url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"
#'
#' landsat <- arc_open(img_url, token = "")
#'
#' bbox <- sf::st_bbox(c(xmin = -71, ymin = 43, xmax = -67, ymax = 47.5), crs = 4326)
#'
#' arc_raster(landsat, bbox, 1000, 1000)
#'
#' }
#'
#' @returns
#'
#' An object of class `SpatRaster`.
#'
#' @export
arc_raster <- function(
    x,
    bbox,
    width = NULL,
    height = NULL,
    format = "tiff",
    token = Sys.getenv("ARCGIS_TOKEN")
) {

  stopifnot(
    "`bbox` must be created with `sf::st_bbox()`" = inherits(bbox, "bbox")
  )
  req <- httr2::request(paste0(x[["url"]], "/exportImage"))

  req <- httr2::req_url_query(
    req,
    bbox = paste0(bbox, collapse = ","),
    bboxSR = validate_crs(sf::st_crs(bbox))[["spatialReference"]][["wkid"]],
    format = format,
    size = paste0(c(width, height), collapse = ","),
    token = token,
    f = "json"
  )

  resp <- httr2::req_perform(req)

  resp_meta <- jsonify::from_json(httr2::resp_body_string(resp))

  detect_errors(resp_meta)

  res <- terra::rast(
    resp_meta$href
  )

  names(res) <- x[["bandNames"]]
  res

}
#
# landsat_url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"
#
# bbox <- sf::st_bbox(c(
#   xmin = -71.087509321293,
#   ymin = 43.091050586836,
#   xmax = -66.969271,
#   ymax = 47.4533344744109
# ), crs = 4326)
#
# x <- image_server(landsat_url)

# res <- query_imagery(x, bbox, width = 5120, height = 5120)
#
# terra::plotRGB(res, 4, 3, 2, scale = 10000)
#
#
#
