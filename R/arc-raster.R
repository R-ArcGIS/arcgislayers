#' Read from an Image Server
#'
#' Given an `ImageServer` export an image as a terra `SpatRaster` object.
#' See [`terra::rast`].
#'
#' @param x an `ImageServer` as created with `arc_open()`.
#' @param xmin the minimum bounding longitude value.
#' @param xmax the maximum bounding longitude value.
#' @param ymin that minimum bounding latitude value.
#' @param ymax the maximum bounding latitude value.
#' @param crs the CRS of the resultant raster image and the provided bounding box defined by `xmin`, `xmax`, `ymin`, `ymax` (passed `outSR` query parameter).
#' @param bbox_crs the CRS of the values passed to `xmin`, `xmax`, `ymin`, and `ymax`.
#'  If not specified, uses the CRS of `x`.
#' @param format default `"tiff"`. Must be one of "jpgpng", "png", "png8", "png24", "jpg", "bmp", "gif", "tiff", "png32", "bip", "bsq", "lerc".
#' @param ... additional key value pairs to be passed to [`httr2::req_body_form()`].
#' @param raster_fn a scalar string with the name of the service raster function. See [`list_service_raster_fns()`] for available raster functions.
#' @param width default `NULL`. Cannot exceed `x[["maxImageWidth"]]`.
#' @param height default `NULL`. Cannot exceed `x[["maxImageHeight"]]`.
#' @param token default `arc_token()` authorization token.
#'
#' @details
#'
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \dontrun{
#' img_url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"
#'
#' landsat <- arc_open(img_url)
#'
#' arc_raster(
#'   landsat,
#'   xmin = -71,
#'   xmax = -67,
#'   ymin = 43,
#'   ymax = 47.5,
#'   bbox_crs = 4326,
#'   width = 100,
#'   height = 100
#' )
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
  ...,
  raster_fn = NULL,
  token = arc_token()
) {
  check_string(raster_fn, allow_null = TRUE)
  if (!is.null(raster_fn)) {
    if (!raster_fn %in% list_service_raster_fns(x)[["name"]]) {
      cli::cli_abort(
        c(
          "{.arg raster_fn} value of {.val {raster_fn}} is not known",
          i = "Use {.fn list_service_raster_fns} to see available raster functions"
        )
      )
    } else {
      raster_fn <- jsonify::to_json(
        list(rasterFunction = raster_fn),
        unbox = TRUE
      )
    }
  }
  # validate and extract CRS object
  out_sr <- validate_crs(crs)[["spatialReference"]][["wkid"]]

  # if bbox_crs is missing set to `crs`
  bbox_sr <- validate_crs(bbox_crs %||% crs)[[c("spatialReference", "wkid")]]

  # create the base req
  burl <- paste0(x[["url"]], "/exportImage")

  # pass that into arc_base_req() to set agent and token
  req <- arc_base_req(burl, token)

  req <- httr2::req_body_form(
    req,
    bbox = paste0(c(xmin, ymin, xmax, ymax), collapse = ","),
    bboxSR = bbox_sr,
    format = format,
    size = paste0(c(width, height), collapse = ","),
    outSR = out_sr,
    ...,
    renderingRule = raster_fn,
    f = "json"
  )

  # fetch the response
  resp <- httr2::req_perform(req)
  resp_meta <- RcppSimdJson::fparse(httr2::resp_body_string(resp))

  detect_errors(resp_meta)

  tmp <- tempfile(fileext = paste0(".", format))
  exported_image_path <- resp_meta[["href"]]
  utils::download.file(exported_image_path, tmp, quiet = TRUE, mode = "wb")

  res <- terra::rast(
    tmp
  )

  names(res) <- x[["bandNames"]]
  res
}
