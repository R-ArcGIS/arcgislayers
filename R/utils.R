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
#' - `pull_field_aliases()` returns a named list of the field aliases from a `FeatureLayer` or `Table`

#' @returns See Details.
#' @export
#' @rdname utils
#' @examples
#' \dontrun{
#' furl <- paste0(
#'   "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/",
#'   "PLACES_LocalData_for_BetterHealth/FeatureServer/0"
#' )
#'
#' flayer <- arc_open(furl)
#'
#' # list fields available in a layer
#' list_fields(flayer)
#'
#' # remove any queries stored in the query attribute
#' clear_query(update_params(flayer, outFields = "*"))
#'
#' # refresh metadata of an object
#' refresh_layer(flayer)
#'
#' map_url <- paste0(
#'   "https://services.arcgisonline.com/ArcGIS/rest/services/",
#'   "World_Imagery/MapServer"
#' )
#'
#' # list all items in a server object
#' list_items(arc_open(map_url))
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

  data_frame(res)
}

#' @export
#' @rdname utils
pull_field_aliases <- function(x) {
  fields <- list_fields(x)

  # get alias values
  rlang::set_names(fields[["alias"]], fields[["name"]])
}

#' @export
#' @rdname utils
list_items <- function(x) {
  check_inherits_any(x, c("FeatureServer", "ImageServer", "MapServer"))
  data_frame(rbind(x[["layers"]], x[["tables"]]))
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
  n_chunks <- ceiling(n / m)
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


#' Check if x and y share the same coordiante reference system
#' @noRd
check_crs_match <- function(
  x,
  y,
  x_arg = rlang::caller_arg(x),
  y_arg = rlang::caller_arg(y),
  call = rlang::caller_env()
) {
  x_crs <- sf::st_crs(x)
  y_crs <- sf::st_crs(y)

  if (x_crs == y_crs) {
    return(invisible(NULL))
  }

  if (!is.na(x_crs) && !is.na(y_crs)) {
    cli::cli_abort(
      c(
        "{.arg {x_arg}} and {.arg {y_arg}} must share the same CRS.",
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

data_frame <- function(x, call = rlang::caller_env()) {
  check_data_frame(x, call = call)
  structure(x, class = c("tbl", "data.frame"))
}

#' @noRd
clear_url_query <- function(url, keep_default = FALSE) {
  query <- parse_url_query(url, keep_default = keep_default)

  if (!is.null(query) && rlang::is_empty(query)) {
    return(url)
  }

  url_elements <- httr2::url_parse(url)

  # Rebuild URL without query
  paste0(
    url_elements[["scheme"]],
    "://",
    url_elements[["hostname"]],
    sub("/query$", "", url_elements[["path"]])
  )
}

#' @noRd
parse_url_query <- function(url, keep_default = FALSE) {
  # Parse url
  url_elements <- httr2::url_parse(url)

  # Return empty list if no query is included in url
  if (is.null(url_elements[["query"]])) {
    return(list())
  }

  # Check for default query values
  query_match <- match(
    url_elements[["query"]],
    c(
      list(outFields = "*", where = "1=1", f = "geojson"),
      list(outFields = "*", where = "1=1")
    )
  )

  # Return NULL for default query
  if (is.numeric(query_match) && !keep_default) {
    return(NULL)
  }

  # Otherwise return query
  url_elements[["query"]]
}

#' List field domains for a layer
#' @noRd
list_field_domains <- function(
  x,
  field = NULL,
  keep_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  fields <- list_fields(x)
  nm <- fields[["name"]]

  if (is.null(nm)) {
    cli::cli_abort("{.arg {x}} must have field names.", call = call)
  }

  domains <- rlang::set_names(fields[["domain"]], nm)

  if (!is.null(field)) {
    field <- rlang::arg_match(field, nm, multiple = TRUE, error_call = call)
    domains <- domains[nm %in% field]
  }

  if (keep_null) {
    return(domains)
  }

  domains[!vapply(domains, is.null, logical(1))]
}

#' Pull a named list of codes for fields using codedValue domain type
#' @noRd
pull_coded_values <- function(
  x,
  field = NULL,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  domains <- list_field_domains(
    x,
    field = field,
    keep_null = FALSE,
    arg = arg,
    call = call
  )

  domains <- lapply(
    domains,
    function(x) {
      if (x[["type"]] != "codedValue") {
        return(NULL)
      }

      values <- x[["codedValues"]]

      rlang::set_names(values[["name"]], values[["code"]])
    }
  )

  Filter(length, domains)
}
