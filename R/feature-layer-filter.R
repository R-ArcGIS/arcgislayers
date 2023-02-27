
# Select ------------------------------------------------------------------


#' @rdname dplyr
select.FeatureLayer <- function(x, ...) {
  selections <- rlang::expr(c(...))
  col_names <- x$fields$name
  names(col_names) <- col_names
  # return(selections)
  select_index <- tidyselect::eval_select(
    selections,
    col_names,
    allow_rename = FALSE
  )

  out_fields <- paste(unname(col_names[select_index]), collapse = ",")

  attr(x, "query")[["outFields"]] <- out_fields
  x
}


#' @rdname dplyr
select.Table <- select.FeatureLayer



# Filter ------------------------------------------------------------------

#' @rdname dplyr

filter.FeatureLayer <- function(x, ...) {

  where_clause <- attr(x, "query")[["where"]]

  filt_quos <- rlang::quos(...)

  ptype_df <- remote_ptype_tbl(x[["fields"]])

  lapply(filt_quos, dbplyr::partial_eval, ptype_df)

  where_clause <- c(where_clause, gsub("`", "", dbplyr::translate_sql(!!!filt_quos)))

  if (length(where_clause) > 1) where_clause <- paste(where_clause, collapse = " AND ")

  attr(x, "query")[["where"]] <- as.character(where_clause)
  x
}

# Table will use same filtering as feature layer. nothing special
#' @rdname dplyr
#' @export
filter.Table <- filter.FeatureLayer



# Collect -----------------------------------------------------------------


#> we want to return all fields if nothing specified
#> so we need to check if null
#> minimum params we need:
#>  - where 1=1
#>  - output = fgeojson
#>  - token

#' dplyr methods
#'
#' @details
#'
#' The Feature Layer method of `collect()` will overwrite the `returnGeometry` parameter if set with `update_params()`. Use the `geometry` argument in `collect()` to set the parameter.
#'
#' @aliases dplyr
#' @rdname dplyr
collect.FeatureLayer <- function(x, token = "", ...) {

  # 1. Make base request
  # 2. Identify necessary query parameters
  # 3. Figure out offsets and update query parameters
  # 4. Make list of requests
  # 5. Make requests
  # 6. Identify errors (if any) -- skip for now
  # 7. Parse:
  #   - if returnGeometry = false, don't use geos
  # 8.
  req <- httr2::request(x[["url"]])

  # stop if query not supported
  if (!grepl("query", x[["capabilities"]], ignore.case = TRUE)) {
    stop("Feature Layer ", x[['name']], " does not support querying")
  }

  # extract existing query
  query <- attr(x, "query")

  # set returnGeometry depending on on geometry arg
  if (is.null(query[["returnGeometry"]])) {
    query[["returnGeometry"]] <- TRUE
  }


  # parameter validation ----------------------------------------------------
  # get existing parameters
  query_params <- validate_params(query, token = token)


  # Offsets -----------------------------------------------------------------
  feats_per_page <- x[["maxRecordCount"]]

  # count the number of features in a query
  n_req <-
    httr2::req_url_query(
      httr2::req_url_query(
        httr2::req_url_path_append(req, "query"),
        !!!query_params
      ),
      returnCountOnly = "true"
    )

  n_feats <- httr2::resp_body_json(
    httr2::req_perform(
      httr2::req_url_query(n_req, f = "pjson")
    ), check_type = FALSE
  )[["count"]]

  # identify the number of pages needed to return all features
  n_pages <- floor(n_feats / feats_per_page)

  # identify the offsets needed to get all pages
  # if n_pages is 0 we set offsets to 0 straight away
  if (n_pages == 0) {
    offsets <- 0
  } else {
    offsets = c(0, (feats_per_page * 1:n_pages) + 1)
  }

  # create a list of requests
  all_requests <- lapply(offsets, add_offset, req, query_params)

  # make all requests and store responses in list
  all_resps <- httr2::multi_req_perform(all_requests)

  # identify any errors
  has_error <- vapply(all_resps, function(x) inherits(x, "error"), logical(1))

  if (any(has_error)) {
  }

  # fetch the results
  res <- lapply(
    all_resps[!has_error],
    function(x) read_fl_page(
      httr2::resp_body_string(x),
      # TODO check for any transformations in the request
      crs = x[["extent"]][["spatialReference"]][["latestWkid"]]
    )
  )

  # combine
  res <- do.call(rbind, res)

  if (inherits(res, "sf")) sf::st_crs(res) <- sf::st_crs(x)
  res
}



# utility -----------------------------------------------------------------

#' Modify Query Parameters
#'
#' @param x a `FeatureLayer` object
#' @param ... key value pairs of query parameters and values.
#' @export
update_params <- function(x, ...) {
  query <- attr(x, "query")

  params <- rlang::list2(...)

  for (name in names(params)) {
    query[[name]] <- params[[name]]
  }

  attr(x, "query") <- query
  x
}

#' This function takes a list of query parameters and creates a query request
#' Importantly, this creates the paginated results that will be needed for
#' Feature Layers with more than 2000 observations
#' @keywords internal
add_offset <- function(offset, request, params) {
  params[["resultOffset"]] <- offset
  req <- httr2::req_url_path_append(request, "query")
  httr2::req_url_query(req, !!!params)
}



validate_params <- function(params, token) {
  # set the token
  params[["token"]] <- token

  # if output fields are missing set to "*"
  if (is.null(params[["outFields"]])) params[["outFields"]] <- "*"

  # if where is missing set it to 1=1
  if (is.null(params[["where"]])) params[["where"]] <- "1=1"

  # set output type to geojson if we return geometry, json if not
  if (is.null(params[["returnGeometry"]]) || isTRUE(params[["returnGeometry"]])) {
    params[["f"]] <- "pgeojson"
  } else {
    params[["f"]] <- "pgeojson"
  }

  params
}
