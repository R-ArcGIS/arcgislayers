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
#' # define the feature layer url
#' furl <- paste0(
#'   "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest",
#'   "/services/PLACES_LocalData_for_BetterHealth/FeatureServer/0"
#' )
#'
#' flayer <- arc_open(furl)
#'
#' arc_select(
#'   flayer,
#'   fields = c("StateAbbr", "TotalPopulation")
#' )
#'
#' arc_select(
#'   flayer,
#'   fields = c("OBJECTID", "PlaceName"),
#'   where = "TotalPopulation > 1000000"
#' )
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
  error_call <- rlang::caller_call()
  # Developer note:
  # For this function we extract the query object and manipulate the elements
  # inside of the query object to modify our request. We then splice those
  # values back into `x` and send our request
  # note that everything that goes into our query must be the json that will
  # be sent directly to the API request which is why we convert it to json
  # before we use `update_params()`
  check_inherits_any(x, c("FeatureLayer", "Table", "ImageServer"))
  check_number_whole(n_max, min = 0, allow_infinite = TRUE)
  check_string(where, allow_null = TRUE, allow_empty = FALSE)
  check_character(fields, allow_null = TRUE)

  # determine if the layer can query
  can_query <- switch(
    class(x)[1],
    "FeatureLayer" = grepl("query", x[["capabilities"]], ignore.case = TRUE),
    "Table" = grepl("query", x[["capabilities"]], ignore.case = TRUE),
    "ImageServer" = x[["supportsAdvancedQueries"]],
    FALSE
  )

  # throw warning if the layer cannot query
  if (!can_query) {
    cli::cli_alert_danger(
      "{class(x)} {.val {x[['name']]}} does not support querying"
    )
  }

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
    check_query_value(val, arg = key, allow_empty = FALSE)

    # insert into query
    query[[key]] <- val
  }

  # handle fields and where clause if missing
  fields <- fields %||% query[["outFields"]]

  # make sure that fields actually exist
  fields <- match_fields(
    fields = fields,
    values = c(x[["fields"]][["name"]], "")
  )

  # include the fields the query
  query[["outFields"]] <- fields

  # include the where clause if present
  query[["where"]] <- where %||% query[["where"]]

  # set returnGeometry depending on on geometry arg
  query[["returnGeometry"]] <- geometry

  # handle filter geometry if not missing
  if (!is.null(filter_geom) && inherits(x, "FeatureLayer")) {
    spatial_filter <- prepare_spatial_filter(
      filter_geom,
      crs = crs,
      predicate = predicate
    )
    # append spatial filter fields to the query
    query <- c(query, spatial_filter)
  } else if (!is.null(filter_geom)) {
    # warn if filter_geom is supplied but object is not a FeatureLayer
    cli::cli_warn(
      "{.arg filter_geom} is ignored when {.arg x} is
      {.obj_simple_type {.cls {class(x)}}}."
    )

    filter_geom <- NULL
  }

  # handle SR if not missing
  if (!is.na(crs)) {
    query[["outSR"]] <- jsonify::to_json(validate_crs(crs)[[1]], unbox = TRUE)
  }

  # update the parameters based on our query list
  x <- update_params(x, !!!query)

  # sets token and agent
  req <- arc_base_req(x[["url"]], token)

  # extract existing query
  query <- attr(x, "query")

  # if the outSR isn't set, set it to be the same as x
  if (inherits(x, "FeatureLayer") && is.null(query[["outSR"]])) {
    query[["outSR"]] <- jsonify::to_json(
      validate_crs(sf::st_crs(x))[[1]],
      unbox = TRUE
    )
  }

  # retain outFields vector and create flag
  out_fields <- query[["outFields"]]
  has_out_fields <- !is.null(out_fields) && !identical(out_fields, "*")

  # determine_format() chooses between pbf and json
  out_f <- determine_format(x, call = error_call)

  # TODO: give this a better name
  query_params <- validate_params(query, out_f)

  # Offsets -----------------------------------------------------------------

  # count the number of features in a query
  n_feats <- count_results(
    req = req,
    query = query_params,
    n_max = n_max,
    error_call = error_call
  )

  all_resps <- get_query_resps(
    x = x,
    req = req,
    n_feats = n_feats,
    page_size = page_size,
    query_params = query_params,
    error_call = error_call
  )

  # process all of the responses
  # uses arcpbf if protocol buffers are supported
  if (out_f == "pbf") {
    res <- arcpbf::resps_data_pbf(all_resps)
  } else {
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
  }

  # Drop fields that aren't selected to avoid returning OBJECTID when not
  # selected
  if (rlang::is_named(res) && has_out_fields) {
    out_fields <- c(out_fields, attr(res, "sf_column"))
    match_nm <- match(tolower(out_fields), tolower(names(res)))
    res <- res[, match_nm[!is.na(match_nm)], drop = FALSE]
  }

  # if the result is empty we return a nothing with a message
  if (rlang::is_empty(res)) {
    cli::cli_alert_info("No features returned from query")
    return(res)
  }

  # we ensure that the CRS is added
  if (inherits(res, "sf") && is.na(sf::st_crs(res))) {
    sf::st_crs(res) <- sf::st_crs(x)
  }

  # ensure that geometry is dropped if geometry is set to false
  # sometimes empty geometry is returned
  if (inherits(res, "sf") && !query[["returnGeometry"]]) {
    res <- sf::st_drop_geometry(res)
  }

  # emit a message if the number of rows is less than what we counted
  if (nrow(res) < n_feats) {
    # See https://github.com/R-ArcGIS/arcgislayers/issues/110
    cli::cli_warn(
      c(
        "Results include fewer than the expected {n_feats} features.",
        "*" = "Try setting {.arg page_size} to a smaller value to make
        sure results include all available features."
      )
    )
  }

  res
}


#' Fetch all query responses
#' @noRd
get_query_resps <- function(
  req,
  x,
  n_feats,
  page_size = NULL,
  query_params = list(),
  error_call = rlang::caller_env()
) {
  # create a list of record counts based on number of features, page size and max records
  record_offsets <- set_record_offsets(
    n_feats = n_feats,
    page_size = page_size,
    max_records = x[["maxRecordCount"]],
    error_call = error_call
  )

  # create a list of requests from the offset and page sizes
  all_requests <- mapply(
    add_offset,
    .offset = record_offsets[["offsets"]],
    .page_size = record_offsets[["counts"]],
    MoreArgs = list(.req = req, .params = query_params),
    SIMPLIFY = FALSE
  )

  # make all requests and store responses in list
  httr2::req_perform_parallel(all_requests, on_error = "continue")
}


# utility -----------------------------------------------------------------

#' Check if an object is a FeatureLayer or Table object
#'
#' obj_check_layer() errors if an object does not inherit either the
#' FeatureLayer or Table class.
#'
#' @param x A `FeatureLayer` or `Table` class object created with [arc_open()].
#' @inheritParams rlang::args_error_context
#' @keywords internal
#' @noRd
obj_check_layer <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
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
#' check_inherits_any() wraps [rlang::inherits_any()] to error if an object
#' does not inherit any of a set of classes.
#'
#' @inheritParams cli::cli_vec
#' @inheritParams rlang::inherits_any
#' @inheritParams rlang::args_error_context
#' @keywords internal
#' @noRd
check_inherits_any <- function(
  x,
  class,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
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
#' furl <- paste0(
#'   "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/",
#'   "USA_Major_Cities_/FeatureServer/0"
#' )
#'
#' flayer <- arc_open(furl)
#' update_params(flayer, outFields = "NAME")
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
#' add_offset() takes a list of query parameters and creates a query request.
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
#' validate_params() ensures that the parameters are set to minimally
#' acceptable values.
#'
#' @keywords internal
#' @noRd
validate_params <- function(params, f = "json") {
  if (!is.null(params[["outFields"]])) {
    params[["outFields"]] <- paste0(params[["outFields"]], collapse = ",")
  } else {
    # if output fields are missing set to "*"
    params[["outFields"]] <- "*"
  }

  # if where is missing set it to 1=1
  params[["where"]] <- params[["where"]] %||% "1=1"

  # set output type to geojson if we return geometry, json if not
  if (
    is.null(params[["returnGeometry"]]) || isTRUE(params[["returnGeometry"]])
  ) {
    params[["f"]] <- f
  } else {
    params[["f"]] <- f
  }

  params
}

# Given a query, determine how many features will be returned
#' @noRd
count_results <- function(
  req,
  query,
  n_max = Inf,
  error_call = rlang::caller_env()
) {
  n_req <- httr2::req_body_form(
    httr2::req_url_path_append(req, "query"),
    # count results should always use json
    !!!validate_params(query, query[["f"]]),
    returnCountOnly = "true"
  )

  resp <- httr2::resp_body_string(
    httr2::req_perform(
      httr2::req_url_query(n_req, f = "json"),
      error_call = error_call
    )
  )

  n_results <- RcppSimdJson::fparse(resp)[["count"]]

  if (is.null(n_results)) {
    cli::cli_abort(
      c(
        "Can't determine the number of requested features.",
        "i" = "Did you set custom parameters via {.arg ...} or
        use an invalid {.arg where} argument?"
      ),
      call = error_call
    )
  }

  if (!is.infinite(n_max) && (n_max < n_results)) {
    cli::cli_alert_info(
      "Query results limited to {n_max} out of {n_results} available feature{?s}."
    )
    n_results <- n_max
  }

  if (!rlang::is_integerish(n_results, 1)) {
    cli::cli_abort(
      c(
        "Can't determine the number of requested features.",
        "*" = "Set {.arg n_max} or check to make sure query parameters are valid."
      ),
      call = error_call
    )
  }
  n_results
}


#' Match fields
#'
#' match_fields() ensures that fields passed to [arc_select()] match
#' permissible values.
#'
#' @keywords internal
#' @noRd
match_fields <- function(
  fields,
  values = NULL,
  multiple = TRUE,
  error_arg = rlang::caller_arg(fields),
  error_call = rlang::caller_env()
) {
  if (is.null(fields) || identical(fields, "*")) {
    return(fields)
  }

  if (all(tolower(fields) %in% tolower(values))) {
    return(fields)
  }

  rlang::arg_match(
    fields,
    values = values,
    multiple = multiple,
    error_arg = error_arg,
    error_call = error_call
  )
}

#' Set record counts to retrieve based on page size and number of pages
#' @noRd
set_record_offsets <- function(
  n_feats = NULL,
  page_size = NULL,
  max_records = NULL,
  error_call = rlang::caller_env()
) {
  # set page size based on the maximum allowed to be returned
  page_size <- validate_page_size(
    page_size,
    max_records = max_records,
    error_call = error_call
  )

  # calculate the total number of requests to be made
  n_pages <- ceiling(n_feats / page_size)
  # these values get passed to `resultOffset`
  offsets <- (1:n_pages - 1) * page_size
  # create vector of page sizes to be passed to `resultRecordCount`
  counts <- rep(page_size, n_pages)
  # modify the last offset to have `resultRecordCount` of the remainder
  # this lets us get an exact value
  counts[n_pages] <- n_feats - offsets[n_pages]

  list(
    "offsets" = offsets,
    "counts" = counts
  )
}

#' Set page size and check page size validity
#'
#' @noRd
validate_page_size <- function(
  page_size = NULL,
  max_records = NULL,
  error_call = rlang::caller_env()
) {
  if (is.numeric(page_size)) {
    # coerce to integer if page_size is numeric
    page_size <- as.integer(page_size)
  }

  check_number_whole(page_size, min = 1, allow_null = TRUE, call = error_call)
  check_number_whole(
    page_size,
    # bug in the standalone checks
    # needs to be a double and cannot be used with
    # max at the same time which is why it is brought into two calls
    max = as.double(max_records),
    allow_null = TRUE,
    call = error_call
  )

  # if page_size is null, use max records (default)
  page_size <- page_size %||% max_records

  if (is.numeric(max_records) && (page_size > max_records)) {
    cli::cli_abort(
      "{.arg page_size} ({page_size}) can't be more than than the layer
      {.field maxRecordCount} property ({max_records}).",
      call = error_call
    )
  }

  page_size
}


# Protocol Buffer helpers ------------------------------------------------
determine_format <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_call()
) {
  check_inherits_any(
    x,
    class = c("FeatureLayer", "Table", "ImageServer"),
    arg = arg,
    call = call
  )

  # extract supported query formats
  query_formats_raw <- x[["supportedQueryFormats"]]

  # perform a check to make sure the supported query formats are
  # actually there if not return false. This shouldn't happen though.
  if (is.null(query_formats_raw)) {
    cli::cli_alert_warning("Cannot determine supported query formats.")
    return("json")
  }

  # split and convert to lower case
  formats <- tolower(strsplit(query_formats_raw, ", ")[[1]])
  if (is.null(formats)) {
    return("json")
  }

  if ("pbf" %in% formats) {
    "pbf"
  } else {
    "json"
  }
}
