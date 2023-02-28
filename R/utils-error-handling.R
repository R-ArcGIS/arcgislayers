#' Detect errors in parsed json response
#'
#' The requests responses from ArcGIS don't return the status code
#' in the response itself but rather from the body in the json.
#' So this function checks for the existence of the error field.
#' @keywords internal
detect_errors <- function(response) {
  if (!is.null(response[["error"]])) {
    stop(c(
      "Status code: ", response[["error"]][["code"]], "\n",
      "  Error ", response$error$messageCode, ": ", response$error$message
    ))
  }
}



