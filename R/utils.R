#' Clear all query parameters
#' @export
#' @rdname utils
clear_query <- function(x) {
  attr(x, "query") <- list()
  x
}

#' List fields in a a feature layer
#'
#' @param x and object of class `FeatureLayer`, `Table`, or `ImageServer`.
#' @export
#' @rdname utils
list_fields <- function(x) {
  res <- x[["fields"]]

  if (is.null(res)) {
    res <- infer_esri_type(data.frame())
  }

  res
}


#' Refreshes an Item's metadata
#'
#' Useful to update metadata after modifying a remote.
#'
#' @export
#' @rdname utils
refresh_layer <- function(x) {
  query <- attr(x, "query")
  xurl <- x[["url"]]
  x <- switch(
    class(x)[1],
    FeatureLayer = feature_layer(xurl),
    Table = feature_table(xurl)
  )

  attr(x, "query") <- query
  x
}
