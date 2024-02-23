#' Utility functions
#'
#' @inheritParams arc_select
#' @details
#'
#' `r lifecycle::badge("experimental")`
#'
#' - `list_fields()` returns a data.frame of the fields in a `FeatureLayer` or `Table`
#' - `list_items()` returns a data.frame containing the layers or tables in a `FeatureServer` or `MapServer`
#' - `clear_query()` removes any saved query in a `FeatureLayer` or `Table` object
#' - `refresh_layer()` syncs a `FeatureLayer` or `Table` with the remote
#'    resource picking up any changes that may have been made upstream.
#'    Returns an object of class `x`.
#'
#' @returns See Details.
#' @export
#' @rdname utils
#' @examples
#' if (interactive()) {
#'   furl <- paste0(
#'     "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/",
#'     "PLACES_LocalData_for_BetterHealth/FeatureServer/0"
#'   )
#'
#'   flayer <- arc_open(furl)
#'
#'   # list fields available in a layer
#'   list_fields(flayer)
#'
#'   # remove any queries stored in the query attribute
#'   clear_query(update_params(flayer, outFields = "*"))
#'
#'   # refresh metadata of an object
#'   refresh_layer(flayer)
#'
#'   map_url <- paste0(
#'     "https://services.arcgisonline.com/ArcGIS/rest/services/",
#'     "World_Imagery/MapServer"
#'   )
#'
#'   # list all items in a server object
#'   list_items(arc_open(map_url))
#'
#' }
clear_query <- function(x) {
  attr(x, "query") <- list()
  x
}

#' @export
#' @rdname utils
list_fields <- function(x) {
  check_inherits_any(x, c("FeatureLayer", "Table", "ImageServer"))
  res <- x[["fields"]]
  if (is.null(res)) {
    res <- infer_esri_type(data.frame())
  }

  res
}

#' @export
#' @rdname utils
list_items <- function(x) {
  check_inherits_any(x, c("FeatureServer", "ImageServer", "MapServer"))
  rbind(x[["layers"]], x[["tables"]])
}

#' @export
#' @rdname utils
refresh_layer <- function(x) {
  check_inherits_any(x, c("FeatureLayer", "Table", "ImageServer"))
  query <- attr(x, "query")
  xurl <- x[["url"]]
  x <- arc_open(xurl)
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
#' @noRd
chunk_indices <- function(n, m) {
  n_chunks <- ceiling(n/m)
  chunk_starts <- seq(1, n, by = m)
  chunk_ends <- seq_len(n_chunks) * m
  chunk_ends[n_chunks] <- n
  list(start = chunk_starts, end = chunk_ends)
}

#' Pick first non-missing CRS
#'
#' @param x an object of class `crs`
#' @param y an object of class `crs`
#'
#' @examples
#'
#' x <- sf::st_crs(27572)
#' y <- sf::st_crs(NA)
#'
#' coalesce_crs(x, y)
#' @noRd
coalesce_crs <- function(x, y) {
  # DEVELOPER NOTE: there is no inheritance check for CRS class
  # I don't know how we would provide an informative error here.
  # dont mess up!
  x_na <- is.na(x)
  y_na <- is.na(y)

  if (x_na && y_na) {
    return(x)
  } else if (y_na) {
    return(x)
  } else if (x_na) {
    return(y)
  } else {
    x
  }
}

#' Does x match the pattern of a URL?
#' @noRd
is_url <- function(x, pattern = NULL, ...) {

  if (!rlang::is_vector(x) || rlang::is_empty(x) || !rlang::is_scalar_character(x)) {
    return(FALSE)
  }

  url_pattern <-
    "http[s]?://(?:[[:alnum:]]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

  if (is.null(pattern)) {
    return(grepl(url_pattern, x, ...))
  }

  grepl(url_pattern, x, ...) & grepl(pattern, x, ...)
}

#' Check if x is a valid URL
#' @noRd
check_url <- function(
    x,
    pattern = NULL,
    ...,
    allow_null = FALSE,
    arg = rlang::caller_arg(url),
    call = rlang::caller_env()) {
  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  if (is_url(x, pattern = pattern)) {
    return(invisible(NULL))
  }

  check_string(
    x,
    allow_empty = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )

  cli::cli_abort(
    "{.arg {arg}} must be a valid url, not {.obj_type_friendly {x}}.",
    call = call
  )
}

#' Check if x is a string
#' @noRd
check_string <- function(
    x,
    allow_empty = TRUE,
    allow_null = FALSE,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()
) {

  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  message <- "{.arg {arg}} must be a scalar character vector."

  if (rlang::is_scalar_character(x)) {
    if (allow_empty || x != "") {
      return(invisible(NULL))
    }

    message <- '{.arg {arg}} must be a non-empty string.'
  }

  cli::cli_abort(
    message,
    call = call
  )
}

#' Check if x and y share the same coordiante reference system
#' @noRd
check_crs_match <- function(x, y, x_arg = rlang::caller_arg(x), y_arg = rlang::caller_arg(y), call = rlang::caller_env()) {
  x_crs <- sf::st_crs(x)
  y_crs <- sf::st_crs(y)

  if (x_crs == y_crs) {
    return(invisible(NULL))
  }

  if (!is.na(x_crs) && !is.na(y_crs)) {
    cli::cli_abort(
      c("{.arg {x_arg}} and {.arg {y_arg}} must share the same CRS.",
        "*" = "Tranform {.arg {y_arg}} to the same CRS as {.arg {x_arg}} with
          {.fn sf::st_transform}"
      ),
      call = call
    )
  }

  if (is.na(y_crs)) {
    cli::cli_warn("{.arg {y_arg}} CRS is missing.")
  }

  if (is.na(x_crs)) {
    cli::cli_warn("{.arg {x_arg}} CRS is missing.")
  }
}

