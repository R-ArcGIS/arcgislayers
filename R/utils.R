#' Remove any null elements from a list
# from https://github.com/r-lib/httr2/blob/87011c0ff31019409c4a5a700b041f279f054361/R/compat-purrr.R
#' @keywords internal
compact <- function(.x) Filter(length, .x)

#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a


# Taken directly from purrr
transpose <- function(.l, .names = NULL) {
  transpose_cpp(.l, .names)
}
