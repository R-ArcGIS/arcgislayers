#' List Available Raster Funcitons
#'
#' This function returns the `rasterFunctionInfos` field of the `ImageServer`'s metadata
#' as a `data.frame`. If the field does not exist then an error is emitted.
#'
#' @inheritParams arcgisutils::infer_esri_type
#' @param x an `ImageServer`.
#' @returns a data.frame of the available raster functions.
#' @export
#' @examples
#' # use paste to avoid cran note
#' furl <- paste0(
#'   "https://di-usfsdata.img.arcgis.com/arcgis/rest/services",
#'   "/FIA_BIGMAP_2018_Species_Aboveground_Biomass/ImageServer"
#' )
#' 
#' service <- arc_open(furl)
#' raster_fns <- list_service_raster_fns(service)
#' head(raster_fns)
list_service_raster_fns <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_call()) {
  check_inherits_any(x, "ImageServer")

  if (!x$allowRasterFunction) {
    cli::cli_abort("{.arg arg} does not support raster functions")
  }
  data_frame(x$rasterFunctionInfos)
}
