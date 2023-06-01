#' Cleary all query parameters
#'
clear_query <- function(x) {
  attr(x, "query") <- list()
  x
}

#' List fields in a a feature layer
#'
#'@param x and object of class `FeatureLayer`, `Table`, or `ImageServer`.
list_fields <- function(x) {
  res <- x[["fields"]]

  if (is.null(res)) {
    res <- infer_esri_type(data.frame())
  }

  res
}


#' Refresh layer
#'
#' Useful to update metadata after modifying a remote
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
