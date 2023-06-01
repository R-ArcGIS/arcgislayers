#' Retrieve Imagery
#'
#' Given an `ImageServer` export an image as a `SpatRaster` from the terra package.
#' See [`terra::rast`].
#'
#' @param x an `ImageServer` as created with `image_server()`.
#' @param xmin the minimum bounding longitude value.
#' @param xmax the maximum bounding longitude value.
#' @param ymin that minimum bounding latitude value.
#' @param ymax the maximum bounding latitude value.
#' @param crs the CRS of the resultant raster image and the provided bounding box defined by `xmin`, `xmax`, `ymin`, `ymax` (passed to `bboxSR` and `outSR` query parameters).
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
    xmin,
    xmax,
    ymin,
    ymax,
    bbox_crs = NULL,
    crs = sf::st_crs(x),
    width = NULL,
    height = NULL,
    format = "tiff",
    token = Sys.getenv("ARCGIS_TOKEN")
) {


  # validate and extract CRS object
  out_sr <- validate_crs(crs)[["spatialReference"]][["wkid"]]

  # if bbox_crs is missing set to `crs`
  bbox_sr <- validate_crs(bbox_crs %||% crs)[[c("spatialReference", "wkid")]]

  req <- httr2::request(paste0(x[["url"]], "/exportImage"))

  req <- httr2::req_url_query(
    req,
    # bbox = paste0(bbox, collapse = ","),
    bbox = paste0(c(xmin, ymin, xmax, ymax), collapse = ","),
    bboxSR = bbox_sr,
    format = format,
    size = paste0(c(width, height), collapse = ","),
    token = token,
    outSR = out_sr,
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
