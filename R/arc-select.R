#' Retrieve a feature layer
#'
#' Give a `FeatureLayer` or `Table` object, retrieve its data as an `sf` object or `tibble` resepctively.
#'
#' @param x an object of class `FeatureLayer` or `Table`.
#' @param fields a character vector of the field names that you wish to be returned. By default all fields are returned.
#' @param where a simple SQL where statement indicating which features should be selected.
#' @param crs the spatial reference to be returned. If the CRS is different than the `FeatureLayer`'s CRS, a transformation will occur server-side. Ignored for `Table` objects.
#' @param filter_geom an object of class `bbox`, `sfc` or `sfg` used to filter
#'   query results based on a predicate function. If an `sfc` object is provided
#'   it will be transformed to the layers spatial reference. If the `sfc` is
#'   missing a CRS (or is an `sfg` object) it is assumed to be in the layers
#'   spatial reference. If an `sfc` object has multiple features, the features
#'   are unioned with [sf::st_union()]. If an `sfc` object has MULTIPOLYGON
#'   geometry, the features are treated as polygonal coverage and unioned with
#'   `is_coverage = TRUE` before being cast to POLYGON geometry with
#'   [sf::st_cast()].
#' @param predicate default `"intersects"`. Possible options are `"intersects"`,  `"contains"`,  `"crosses"`,  `"overlaps"`,  `"touches"`, and `"within"`.
#' @param n_max the maximum number of features to return. By default returns every feature available. Unused at the moment.
#' @param ... additional query parameters passed to the API. See [reference documentation](https://developers.arcgis.com/rest/services-reference/enterprise/query-feature-service-layer-.htm#GUID-BC2AD141-3386-49FB-AA09-FF341145F614) for possible arguments.
#' @export
arc_select <- function(
    x,
    fields,
    where,
    crs = sf::st_crs(x),
    filter_geom,
    predicate = "intersects",
    n_max = Inf,
    ...
) {

  obj_check_layer(x)

  if (!missing(filter_geom)) {
    x <- apply_filter_geom(
        x,
        filter_geom = filter_geom,
        predicate = predicate,
        crs = crs
      )
  }

  query <- attr(x, "query")

  # handle fields and where clause if missing
  if (missing(fields)) {
    fields <- query[["outFields"]] %||% "*"
  }

  # if not missing fields collapse to scalar character
  if (!missing(fields) && (length(fields) > 1)) {
    # check if incorrect field names provided
    x_fields <- x[["fields"]][["name"]]
    nindex <- tolower(fields) %in% tolower(x_fields)

    if (any(!nindex)) {
      cli::cli_abort(
        "Field{?s} not in {.arg x}: {.var {fields[!nindex]}}"
      )
    }

    fields <- paste0(fields, collapse = ",")
  }

  # if where is missing set to 1=1
  if (missing(where)) {
    where <- query[["where"]] %||% "1=1"
  }

  # handle SR
  if (is.na(crs)) {
    out_sr <- NULL
  } else {
    out_sr <- jsonify::to_json(validate_crs(crs)[[1]], unbox = TRUE)
  }

  x <- update_params(
      x,
      outFields = fields,
      where = where,
      outSR = out_sr,
      ...
    )

  collect_layer(x, n_max = n_max)
}

#' Apply filter_geom to x
#'
#' @keywords internal
apply_filter_geom <- function(x,
                              filter_geom = NULL,
                              predicate = "intersects",
                              crs = sf::st_crs(x),
                              error_call = rlang::caller_env()) {
  check_inherits_any(
    filter_geom,
    class = c("sfc", "sfg", "bbox"),
    call = error_call
  )

  if (inherits(filter_geom, "bbox")) {
    filter_geom <- sf::st_as_sfc(filter_geom)
  }

  if (inherits(filter_geom, "sfg")) {
    filt_crs <- crs
  }

  # if its an sfc object it must be length one
  if (inherits(filter_geom, "sfc")) {
    if (length(filter_geom) > 1) {
      filter_geom <- sf::st_union(filter_geom)
    }

    # if the CRS is missing use the layer's CRS
    # otherwise use the CRS of the filter_geom object
    filt_crs <- sf::st_crs(filter_geom)

    if (is.na(filt_crs)) {
      filt_crs <- crs
    }

    # extract the sfg object which is used to write Esri json
    filter_geom <- filter_geom[[1]]
  }

  # if a multi polygon stop, must be a single polygon see
  # related issue: https://github.com/R-ArcGIS/arcgislayers/issues/4
  if (inherits(filter_geom, "MULTIPOLYGON")) {
    cli::cli_inform(
      c(
        "!" = "{.arg filter_geom} can't have {.val MULTIPOLYGON} geometry.",
        "i" = "Using {.fn sf::st_union} and {.fn sf::st_cast} to create a
        coverage {.val POLYGON} for {.arg filter_geom}."
      ),
      call = error_call
    )

    filter_geom <- sf::st_union(filter_geom, is_coverage = TRUE)
    filter_geom <- sf::st_cast(filter_geom, to = "POLYGON")
  }

  esri_geometry <- as_esri_geometry(filter_geom, crs = filt_crs)

  filter_params <- list(
    geometryType = determine_esri_geo_type(filter_geom),
    geometry = esri_geometry,
    spatialRel = match_spatial_rel(predicate)
    # TODO is `inSR` needed if the CRS is specified in the geometry???
  )

  update_params(x, rlang::splice(filter_params))
}

# This is the workhorse function that actually executes the queries
#' @keywords internal
collect_layer <- function(x, n_max = Inf, token = Sys.getenv("ARCGIS_TOKEN"), ..., error_call = rlang::caller_env()) {
  obj_check_layer(x, call = error_call)

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
    cli::cli_abort(
      "{class(x)} {.val {x[['name']]}} does not support querying",
      call = error_call
    )
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
        httr2::req_url_query(n_req, f = "pjson"),
        error_call = error_call
      ), check_type = FALSE
    )[["count"]]
  )

  if (is.null(n_feats)) {
    cli::cli_abort(
      c("Can't determine the number of features for {.arg x}.",
      "*" = "Check to make sure your {.arg where} statement is valid."),
      call = error_call
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
  # TODO: determine how to handle errors
  #   }

  # fetch the results
  res <- lapply(
    all_resps[!has_error],
    function(x) {
      parse_esri_json(
        httr2::resp_body_string(x)
        )
    }
  )

  # combine
  res <- do.call(rbind, res)

  if (is.null(res)) {
    cli::cli_warn(
      "No features returned from query",
      call = error_call
    )
    return(data.frame())
  }

  if (inherits(res, "sf")) sf::st_crs(res) <- sf::st_crs(x)

  res

}


# utility -----------------------------------------------------------------

#' Check if x is a FeatureLayer or Table class object
#' @keywords internal
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

#' Check if x inherits any of the supplied class values and error if not
#' @inheritParams cli::cli_vec
#' @inheritParams rlang::inherits_any
#' @inheritParams rlang::args_error_context
#' @keywords internal
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
    "{.arg {arg}} must be a {class} object, not {.obj_simple_type {x}}.",
    call = call
  )
}

# This is the function that takes named arguments and updates the query
#' Modify Query Parameters
#'
#' @param x a `FeatureLayer` object
#' @param ... key value pairs of query parameters and values.
#' @keywords internal
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


# This function ensures that the minimal parameters are correctly set

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
    params[["f"]] <- "json"
  } else {
    params[["f"]] <- "json"
  }

  params
}


