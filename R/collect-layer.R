collect_layer <- function(x, n_max = Inf, token = Sys.getenv("ARCGIS_TOKEN"), ...) {

  if (!inherits(x, c("FeatureLayer", "Table")))
    stop("`x` must be a `FeatureLayer` or `Table` object")
  # 1. Make base request
  # 2. Identify necessary query parameters
  # 3. Figure out offsets and update query parameters
  # 4. Make list of requests
  # 5. Make requests
  # 6. Identify errors (if any) -- skip for now
  # 7. Parse:
  req <- httr2::request(x[["url"]])

  # stop if query not supported
  if (!grepl("query", x[["capabilities"]], ignore.case = TRUE)) {
    stop("Feature Layer ", x[['name']], " does not support querying")
  }

  # extract existing query
  query <- attr(x, "query")

  # set returnGeometry depending on on geometry arg and is FeatureLayer
  if (is.null(query[["returnGeometry"]]) && inherits(x, "FeatureLayer")) {
    query[["returnGeometry"]] <- TRUE
  }

  # if the outSR isn't set, set it to be the same as x
  if (inherits(x, "FeatureLayer") && is.null(query[["outSR"]])) {
    query[["outSR"]] <- jsonify::to_json(validate_crs(sf::st_crs(x))[[1]], unbox = TRUE)
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
        !!!query_params[c("where", "outFields", "f", "token")]
      ),
      returnCountOnly = "true"
    )

  suppressMessages(
    n_feats <- httr2::resp_body_json(
      httr2::req_perform(
        httr2::req_url_query(n_req, f = "pjson")
      ), check_type = FALSE
    )[["count"]]
  )

  if (is.null(n_feats)) {
    stop(
    "Could not determine the number of features.\n  Is your `where` statement invalid?"
    )
  }

  # identify the number of pages needed to return all features
  # if n_max is provided need to reduce the number of pages
  if (n_feats > n_max) {
    n_feats <- n_max
    # set `resultRecordCount` to `n_max`
    query_params[["resultRecordCount"]] <- n_max
  }

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
  #
  #   if (any(has_error)) {
        # TODO determine how to handle errors
  #   }

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



#' @keywords internal
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


