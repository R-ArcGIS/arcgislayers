#' List Available Raster Funcitons
#'
#' This function returns the `rasterFunctionInfos` field of the `ImageServer`'s metadata
#' as a `data.frame`. If the field does not exist then an error is emitted.
#'
#' @inheritParams arcgisutils::infer_esri_type
#' @param x an `ImageServer`.
#' @returns a data.frame of the available raster functions.
#' @examples
#' # example code
#'
list_service_raster_fns <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_call()) {
  check_inherits_any(x, "ImageServer")

  if (!x$allowRasterFunction) {
    cli::cli_abort("{.arg arg} does not support raster functions")
  }
  data_frame(x$rasterFunctionInfos)
}
