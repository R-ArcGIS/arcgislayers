#' Query a Feature Service
#'
#' [arc_select()] takes a `FeatureLayer`, `Table`, of `ImageServer` object and
#' returns data from the layer as an `sf` object or `data.frame` respectively.
#'
#' @param x an object of class `FeatureLayer`, `Table`, or `ImageServer`.
#' @param fields a character vector of the field names that you wish to be
#'   returned. By default all fields are returned.
#' @param where a simple SQL where statement indicating which features should be
#'   selected.
#' @param crs the spatial reference to be returned. If the CRS is different than
#'   the CRS for the input `FeatureLayer`, a transformation will occur
#'   server-side. Ignored if x is a `Table`.
#' @param geometry default `TRUE`. If geometries should be returned. Ignored for
#'   `Table` objects.
#' @inheritParams prepare_spatial_filter
#' @param n_max the maximum number of features to return. By default returns
#'   every feature available. Unused at the moment.
#' @param page_size the maximum number of features to return per request. Useful when requests return a 500 error code. See Details.
#' @param ... additional query parameters passed to the API.
#' @inheritParams arc_open
#'
#' @details
#'
#' See [reference documentation](https://developers.arcgis.com/rest/services-reference/enterprise/query-feature-service-layer-.htm#GUID-BC2AD141-3386-49FB-AA09-FF341145F614) for possible arguments.
#'
#' `FeatureLayers` can contain very dense geometries with a lot of coordinates.
#' In those cases, the feature service may time out before all geometries can
#' be returned. To address this issue, we can reduce the number of features
#' returned per each request by reducing the value of the `page_size` parameter.
#'
#' `arc_select()` works by sending a single request that counts the number of
#' features that will be returned by the current query. That number is then used
#' to calculate how many "pages" of responses are needed to fetch all the results.
#' The number of features returned (page size) is set to the `maxRecordCount`
#' property of the layer by default. However, by setting `page_size` to be
#' smaller than the `maxRecordCount` we can return fewer geometries per page and
#' avoid time outs.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @export
#' @examples
#' \dontrun{
#'   # define the feature layer url
#'   furl <- paste0(
#'     "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest",
#'     "/services/PLACES_LocalData_for_BetterHealth/FeatureServer/0"
#'   )
#'
#'   flayer <- arc_open(furl)
#'
#'   arc_select(
#'     flayer,
#'     fields = c("StateAbbr", "TotalPopulation")
#'   )
#'
#'   arc_select(
#'     flayer,
#'     fields = c("OBJECTID", "PlaceName"),
#'     where = "TotalPopulation > 1000000"
#'   )
#' }
#' @returns An sf object, or a data.frame
arc_select <- function(
    x,
    ...,
    fields = NULL,
    where = NULL,
    crs = sf::st_crs(x),
    geometry = TRUE,
    filter_geom = NULL,
    predicate = "intersects",
    n_max = Inf,
    page_size = NULL,
    token = arc_token()
) {

  # Developer note:
  # For this function we extract the query object and manipulate the elements
  # inside of the query object to modify our request. We then splice those
  # values back into `x` and send our request
  # note that everything that goes into our quey must be the json that will
  # be sent directly to the API request which is why we convert it to json
  # before we use `update_params()`
  check_inherits_any(x, c("FeatureLayer", "Table", "ImageServer"))

  # extract the query object
  query <- attr(x, "query")

  # if dots provided we check that all elements are named
  dots <- rlang::list2(...)
  check_dots_named(dots)

  # extract dots names
  dots_names <- names(dots)

  # insert into query
  for (i in seq_along(dots)) {
    key <- dots_names[i]
    val <- dots[[i]]
    # check that the value is a scalar and non-empty
    check_string(val, allow_empty = FALSE)

    # insert into query
    query[[key]] <- val
  }

  # handle fields and where clause if missing
  fields <- fields %||% query[["outFields"]] %||% "*"

  # if not missing fields collapse to scalar character
  if (length(fields) > 1) {
    # check if incorrect field names provided
    x_fields <- x[["fields"]][["name"]]
    nindex <- tolower(fields) %in% tolower(x_fields)

    # handle the case where a field is being selected that
    # is not one of the available fields in the feature layer
    if (any(!nindex)) {
      cli::cli_abort(
        "Field{?s} not in {.arg x}: {.var {fields[!nindex]}}"
      )
    }
    # collapse together
    fields <- paste0(fields, collapse = ",")
  }

  query[["outFields"]] <- fields

  # if where is missing set to 1=1
  query[["where"]] <- where %||% query[["where"]] %||% "1=1"

  # set returnGeometry depending on on geometry arg
  query[["returnGeometry"]] <- geometry

  # handle filter geometry if not missing
  if (!is.null(filter_geom)) {
    spatial_filter <- prepare_spatial_filter(
      filter_geom,
      crs = crs,
      predicate = predicate
    )

    # append spatial filter fields to the query
    query <- c(query, spatial_filter)
  }

  # handle SR if not missing
  if (!is.na(crs)) {
    query[["outSR"]] <- jsonify::to_json(validate_crs(crs)[[1]], unbox = TRUE)
  }

  # update the parameters based on our query list
  x <- update_params(x, !!!query)

  # send the request
  collect_layer(x, n_max = n_max, token = token, page_size = page_size, ...)
}

#' Query a FeatureLayer or Table object
#'
#' [collect_layer()] is the "workhorse" function that actually executes the
#' queries for FeatureLayer or Table objects.
#'
#' @noRd
collect_layer <- function(
    x,
    n_max = Inf,
    token = arc_token(),
    page_size = NULL,
    ...,
    error_call = rlang::caller_env()
) {

  if (length(page_size) > 1) {
    cli::cli_abort("{.arg page_size} must be length 1 not {length(page_size)}")
  } else if (!is.null(page_size) && page_size < 1) {
    cli::cli_abort("{.arg page_size} must be a positive integer.")
  }

  # 1. Make base request
  # 2. Identify necessary query parameters
  # 3. Figure out offsets and update query parameters
  # 4. Make list of requests
  # 5. Make requests
  # 6. Identify errors (if any) -- skip for now
  # 7. Parse:

  # sets token and agent
  req <- arc_base_req(x[["url"]], token)

  # determine if the layer can query
  can_query <- switch(
    class(x),
    "FeatureLayer" = grepl("query", x[["capabilities"]], ignore.case = TRUE),
    "Table" = grepl("query", x[["capabilities"]], ignore.case = TRUE),
    "ImageServer" = x[["supportsAdvancedQueries"]],
    FALSE
  )

  # throw error if the layer cannot query
  if (!can_query) {
    cli::cli_abort(
      "{class(x)} {.val {x[['name']]}} does not support querying",
      call = error_call
    )
  }

  # extract existing query
  query <- attr(x, "query")

  # if the outSR isn't set, set it to be the same as x
  if (inherits(x, "FeatureLayer") && is.null(query[["outSR"]])) {
    query[["outSR"]] <- jsonify::to_json(validate_crs(sf::st_crs(x))[[1]], unbox = TRUE)
  }

  # parameter validation ----------------------------------------------------
  # get existing parameters
  query_params <- validate_params(query)

  # Offsets -----------------------------------------------------------------

  # get the maximum allowed to be returned
  max_records <- x[["maxRecordCount"]]

  # if its null, just use max records (default)
  if (is.null(page_size)) {
    feats_per_page <- max_records
  } else if (page_size > max_records) {
    cli::cli_abort("{.arg page_size} ({page_size}) cannot excede layer's {.field maxRecordCount} property ({max_records})")
  } else {
    # ensure its an integer.
    page_size <- as.integer(page_size)
    feats_per_page <- page_size
  }

  # count the number of features in a query
  n_feats <- count_results(req, query_params)

  if (is.null(n_feats)) {
    cli::cli_abort(c(
      "Cannot determine the number of features in request.",
      "i" = "did you set custom parameters via {.arg ...}?"
    ), call = error_call)
  }

  # identify the number of pages needed to return all features
  # if n_max is provided need to reduce the number of pages
  if (n_feats > n_max) {
    n_feats <- n_max
  }

  if (is.null(n_feats)) {
    cli::cli_abort(
      c("Can't determine the number of features for {.arg x}.",
      "*" = "Check to make sure your {.arg where} statement is valid."),
      call = error_call
    )
  }

  # calculate the total number of requests to be made
  n_pages <- ceiling(n_feats / feats_per_page)
  # these values get passed to `resultOffset`
  offsets <- (1:n_pages - 1) * feats_per_page
  # create vector of page sizes to be passed to `resultRecordCount`
  record_counts <- rep(feats_per_page, n_pages)
  # modify the last offset to have `resultRecordCount` of the remainder
  # this lets us get an exact value
  record_counts[n_pages] <- n_feats - offsets[n_pages]

  # create a list of requests from the offset and page sizes
  all_requests <- mapply(
    add_offset,
    .offset = offsets,
    .page_size = record_counts,
    MoreArgs = list(.req = req, .params = query_params),
    SIMPLIFY = FALSE
  )

  # make all requests and store responses in list
  all_resps <- httr2::req_perform_parallel(all_requests, on_error = "continue")

  # identify any errors
  # TODO: determine how to handle errors
  # has_error <- vapply(all_resps, function(x) inherits(x, "error"), logical(1))

  # fetch the results
  res <- lapply(
    all_resps,
    # all_resps[!has_error],
    function(x) {
      parse_esri_json(
        httr2::resp_body_string(x),
        call = error_call
      )
    }
  )

  # combine results
  res <- rbind_results(res, call = error_call)

  out_fields <- query[["outFields"]]

  # Drop fields that aren't selected to avoid returning objectID
  if (rlang::is_named(res) && !is.null(out_fields) && !identical(out_fields, "*")) {
    out_fields <- c(out_fields, attr(res, "sf_column"))
    res_nm <- names(res)
    res <- res[ , tolower(res_nm) %in% tolower(out_fields), drop = FALSE]
  }

  if (rlang::is_empty(res)) {
    cli::cli_alert_info("No features returned from query")
    return(res)
  }

  if (inherits(res, "sf") && is.na(sf::st_crs(res))) {
    sf::st_crs(res) <- sf::st_crs(x)
  }

  res

}


# utility -----------------------------------------------------------------

#' Check if an object is a FeatureLayer or Table object
#'
#' [obj_check_layer()] errors if an object does not inherit either the
#' FeatureLayer or Table class.
#'
#' @param x A `FeatureLayer` or `Table` class object created with [arc_open()].
#' @inheritParams rlang::args_error_context
#' @keywords internal
#' @noRd
obj_check_layer <- function(x,
                            arg = rlang::caller_arg(x),
                            call = rlang::caller_env()) {
  check_inherits_any(
    x,
    class = c("FeatureLayer", "Table"),
    arg = arg,
    call = call
  )
}

#' @noRd
obj_is_layer <- function(x) {
  rlang::inherits_any(x, c("FeatureLayer", "Table"))
}

#' Check if an object inherits from a set of classes
#'
#' [check_inherits_any()] wraps [rlang::inherits_any()] to error if an object
#' does not inherit any of a set of classes.
#'
#' @inheritParams cli::cli_vec
#' @inheritParams rlang::inherits_any
#' @inheritParams rlang::args_error_context
#' @keywords internal
#' @noRd
check_inherits_any <- function(x,
                               class,
                               arg = rlang::caller_arg(x),
                               call = rlang::caller_env()) {
  if (rlang::inherits_any(x, class)) {
    return(invisible(NULL))
  }

  class <- cli::cli_vec(
    class,
    style = list("before" = "`", "after" = "`", "vec-last" = " or ")
  )

  cli::cli_abort(
    "{.arg {arg}} must be a {class} object, not {.obj_simple_type {.cls {class(x)}}}.",
    call = call
  )
}

#' Modify query parameters
#'
#' [update_params()] takes named arguments and updates the query.
#'
#' @param x a `FeatureLayer` or `Table` object
#' @param ... key value pairs of query parameters and values.
#' @export
#' @examples
#' \dontrun{
#'   furl <- paste0(
#'     "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/",
#'     "USA_Major_Cities_/FeatureServer/0"
#'   )
#'
#'  flayer <- arc_open(furl)
#'  update_params(flayer, outFields = "NAME")
#' }
#' @returns An object of the same class as `x`
update_params <- function(x, ...) {
  query <- attr(x, "query")
  params <- rlang::list2(...)

  for (name in names(params)) {
    query[[name]] <- params[[name]]
  }

  attr(x, "query") <- query
  x
}

#' Add an offset to a query parameters
#'
#' [add_offset()] takes a list of query parameters and creates a query request.
#' Importantly, this creates the paginated results that will be needed for
#' Feature Layers with more than 2000 observations.
#'
#' @keywords internal
#' @noRd
add_offset <- function(.req, .offset, .page_size, .params) {
  .req <- httr2::req_url_path_append(.req, "query")
  httr2::req_body_form(
    .req,
    !!!.params,
    resultOffset = .offset,
    resultRecordCount = .page_size
  )
}

#' Validate query parameters
#'
#' [validate_params()] ensures that the parameters are set to minimally
#' acceptable values.
#'
#' @keywords internal
#' @noRd
validate_params <- function(params) {

  # if output fields are missing set to "*"
  if (is.null(params[["outFields"]])) params[["outFields"]] <- "*"

  # if where is missing set it to 1=1
  if (is.null(params[["where"]])) params[["where"]] <- "1=1"

  # set output type to geojson if we return geometry, json if not
  if (is.null(params[["returnGeometry"]]) || isTRUE(params[["returnGeometry"]])) {
    params[["f"]] <- "json"
  } else {
    params[["f"]] <- "json"
  }

  params
}

# Given a query, determine how many features will be returned
count_results <- function(req, query) {
  n_req <- httr2::req_body_form(
    httr2::req_url_path_append(req, "query"),
    !!!validate_params(query),
    returnCountOnly = "true"
  )

  resp <- httr2::resp_body_string(
    httr2::req_perform(
      httr2::req_url_query(n_req, f = "json"),
      error_call = rlang::caller_env()
    )
  )

  RcppSimdJson::fparse(resp)[["count"]]
}

