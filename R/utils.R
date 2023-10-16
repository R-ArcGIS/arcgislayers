#' Utility functions
#'
#' @details
#'
#' - `list_fields()` returns a data.frame of the fields in a `FeatureLayer` or `Table`
#' - `list_items()` returns a data.frame containing the layers or tables in a `FeatureServer` or `MapServer`
#' - `clear_query()` removes any saved query in a `FeatureLayer` or `Table` object
#' - `refresh_layer()` syncs a `FeatureLayer` or `Table` with the remote resource picking up any changes that may have been made upstream
#'
#' @export
#' @rdname utils
clear_query <- function(x) {
  attr(x, "query") <- list()
  x
}

#' @export
#' @rdname utils
list_fields <- function(x) {
  res <- x[["fields"]]

  if (is.null(res)) {
    res <- infer_esri_type(data.frame())
  }

  res
}

#' @export
#' @rdname utils
list_items <- function(x) {
  rbind(x[["layers"]], x[["tables"]])
}

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



#' Get chunk indices
#'
#' For a given number of items and a chunk size, determine the start and end
#' positions of each chunk.
#'
#' @param n the number of rows
#' @param m the chunk size
#' @keywords internal
chunk_indices <- function(n, m) {
  n_chunks <- ceiling(n/m)
  chunk_starts <- seq(1, n, by = m)
  chunk_ends <- seq_len(n_chunks) * m
  chunk_ends[n_chunks] <- n
  list(start = chunk_starts, end = chunk_ends)
}
