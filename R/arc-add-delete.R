# Add Features ------------------------------------------------------------

#' Add Features to Feature Layer
#'
#' @param x an object of class `FeatureLayer`
#' @param .data an object of class `sf` or `data.frame`
#' @param chunk_size the maximum number of features to add at a time
#' @param match_on whether to match on the alias or the field name. Default,
#'  the alias. See Details for more.
#' @param rollback_on_failure if anything errors, roll back writes.
#'  Defaults to `TRUE`.
#' @param progress default `TRUE`. A progress bar to be rendered by `httr2` to track requests.
#' @param token your authorization token.
#'
#' @inheritParams arc_select
#' @details
#'
#' `r lifecycle::badge("experimental")`
#'
#' For a more detailed guide to adding, updating, and deleting features, view the
#' tutorial on the [R-ArcGIS Bridge website](https://developers.arcgis.com/r-bridge/editing/overview/).
#'
#' Regarding the `match_on` argument:when publishing an object to an ArcGIS Portal
#' from R, the object's names are provided as the alias. The object's names are
#' subject to change according to the standards of the ArcGIS REST API. For example.
#' `"Sepal.Length"` is changed to `"Sepal_Width"` in the `name` field but the alias
#' remains `"Sepal.Length"`. For that reason, we match on the alias name by default.
#' Change this argument to match based on the field name.
#'
#' @export
#' @rdname modify
#' @examples
#' \dontrun{
#'   # this is pseudo-code and will not work
#'   flayer <- arc_open(furl)
#'
#'   # add sf objects to existing feature service
#'   add_features(flayer, sfobj)
#'
#'   # delete all features
#'   delete_features(flayer, where = "1 = 1")
#'
#'   # update features
#'   update_features(flayer, dfobj)
#' }
#' @returns
#' - `add_features()` returns a `data.frame` with columns `objectId`, `uniqueId`, `globalId`, `success`
#' - `update_features()` returns a list with an element named `updateResults` which is a `data.frame` with columns `objectId`, `uniqueId`, `globalId`, `success`
#' - `delete_features()` returns a list with an element named `deleteResults` which is a `data.frame` with columns `objectId`, `uniqueId`, `globalId`, `success`
add_features <- function(
  x,
  .data,
  chunk_size = 500,
  match_on = c("name", "alias"),
  rollback_on_failure = TRUE,
  progress = TRUE,
  token = arc_token()
) {
  # initial check for type of `x`
  obj_check_layer(x)

  if (!rlang::inherits_any(.data, "data.frame")) {
    data_name <- deparse(substitute(.data))
    cli::cli_abort(
      "{.arg {data_name}} must be a {.cls data.frame}."
    )
  }

  match_on <- match.arg(match_on)

  # DEVELOPER NOTE: date fields and others should be handled
  # in the json converters in arcgisutils as_features and as_featureset

  # TODO separate into multiple different requests
  # TODO address data.frame objects / table layers
  # TODO error on list columns

  check_crs_match(x, .data)

  # not that addFeatures does not update layer definitions so if any attributes
  # are provided that aren't in the feature layer, they will be ignored
  feature_fields <- list_fields(x)
  cnames <- colnames(.data)

  # find which columns are present in the layer
  present_index <- cnames %in% feature_fields[[match_on]]

  # fetch the geometry column name
  geo_col <- attr(.data, "sf_column")

  if (match_on == "alias") {
    lu <- stats::setNames(feature_fields[["name"]], feature_fields[["alias"]])

    # ensure the geo_col is present if its an sf object
    if (inherits(.data, "sf")) {
      cnames[length(cnames)] <- geo_col
    } else {
      cnames <- unname(lu[cnames])
    }
    colnames(.data) <- cnames
  }

  inform_nin_feature(
    # columns not in the feature layer
    setdiff(cnames[!present_index], geo_col)
  )

  # subset accordingly
  .data <- .data[, present_index, drop = FALSE]

  # count the number of rows in a data frame
  n <- nrow(.data)

  # get the indices to chunk them up
  indices <- chunk_indices(n, chunk_size)

  # create the base request from the feature url
  base_req <- arc_base_req(x[["url"]], token)
  req <- httr2::req_url_path_append(base_req, "addFeatures")

  n_chunks <- lengths(indices)[1]
  # pre-allocate list
  all_reqs <- vector("list", length = n_chunks)

  # create a progress bar for chunking
  if (progress) {
    pb <- cli::cli_progress_bar(
      "Chunking features",
      type = "iterator",
      total = n_chunks
    )
  }

  # populate the requests veector
  for (i in seq_along(all_reqs)) {
    if (progress) {
      cli::cli_progress_update(1, id = pb)
    }
    start <- indices[["start"]][i]
    end <- indices[["end"]][i]

    all_reqs[[i]] <- httr2::req_body_form(
      req,
      features = as_esri_features(.data[start:end, , drop = FALSE]),
      rollbackOnFailure = rollback_on_failure,
      f = "json"
    )
  }

  if (progress) {
    cli::cli_progress_done(pb)
  }

  # send the requests in parallel
  all_resps <- httr2::req_perform_parallel(all_reqs, progress = progress)

  # parse the responses into a data frame
  do.call(
    rbind.data.frame,
    lapply(all_resps, function(res) {
      resp <- RcppSimdJson::fparse(
        httr2::resp_body_string(res)
      )

      # check if any errors are present in any of the requests
      detect_errors(resp)

      # return the addResults dataframe
      resp[["addResults"]]
    })
  )
}


#' @noRd
inform_nin_feature <- function(nin_feature, error_call = rlang::caller_call()) {
  if (length(nin_feature) == 0) {
    return(invisible(NULL))
  }

  # TODO fix with pluralisation
  cli::cli_alert_danger(
    paste0(
      "Columns in `.data` not in feature(s): ",
      ifelse(
        length(nin_feature) > 1,
        paste0(nin_feature, collapse = ", "),
        nin_feature
      )
    )
  )

  if (interactive()) {
    cont <- utils::menu(
      c("Yes", "No"),
      title = "Columns not found in feature service. Would you like to continue?"
    )

    if (cont == 2L) {
      cli::cli_abort("Stopping.", call = error_call)
    }
  }
}


# Delete Features ---------------------------------------------------------

#' Delete Features from Feature Layer
#'
#' Delete features from a feature layer based on object ID, a where clause, or a
#' spatial filter.
#'
#' @inheritParams arc_select
#' @param object_ids a numeric vector of object IDs to be deleted.
#' @param where a simple SQL where statement indicating which features should be
#'   deleted. When the where statement evaluates to `TRUE`, those values will be
#'   deleted.
#' @inheritParams prepare_spatial_filter
#' @param rollback_on_failure default `TRUE`. Specifies whether the edits should be
#'   applied only if all submitted edits succeed.
#' @param token default `arc_token()`. An `httr2_token`.
#' @export
#' @rdname modify
delete_features <- function(
  x,
  object_ids = NULL,
  where = NULL,
  filter_geom = NULL,
  predicate = "intersects",
  rollback_on_failure = TRUE,
  chunk_size = 500,
  progress = TRUE,
  token = arc_token()
) {
  obj_check_layer(x)

  # where, oid, or filter_geom necessary
  if (is.null(object_ids) && is.null(where) && is.null(filter_geom)) {
    cli::cli_abort(
      c(
        "No features to delete",
        "i" = "Supply at least one of {.arg object_ids}, {.arg where},
      or {.arg filter_geom}"
      )
    )
  }

  if (!rlang::is_integerish(object_ids) && !is.null(object_ids)) {
    cli::cli_abort(
      "`object_ids` must be an integer vector. Consider casting to integer via {.function as.integer}"
    )
  } else {
    # explicitly ensure that they are integers
    object_ids <- as.integer(object_ids)
  }

  if (!is.null(object_ids)) {
    object_ids <- paste0(object_ids, collapse = ",")
  }

  # convert to the proper CRS if not missing
  if (!rlang::is_null(filter_geom)) {
    # we extract the two CRS object
    # then if filter_geom has no crs we use x via `coalesce_crs()`
    x_crs <- sf::st_crs(x)
    filt_crs <- sf::st_crs(filter_geom)
    crs <- coalesce_crs(filt_crs, x_crs)
    filter_geom <- prepare_spatial_filter(
      filter_geom = filter_geom,
      predicate = predicate,
      crs = crs
    )
  }

  # https://developers.arcgis.com/rest/services-reference/enterprise/delete-features.htm
  req <- arc_base_req(paste0(x[["url"]], "/deleteFeatures"), token)

  n <- length(object_ids)
  indices <- chunk_indices(n, chunk_size)

  n_chunks <- lengths(indices)[1]
  # # pre-allocate list
  all_reqs <- vector("list", length = n_chunks)

  if (progress) {
    pb <- cli::cli_progress_bar(
      "Chunking object ids",
      type = "iterator",
      total = n_chunks
    )
  }

  # # populate the requests veector
  for (i in seq_along(all_reqs)) {
    if (progress) {
      cli::cli_progress_update(1, id = pb)
    }
    start <- indices[["start"]][i]
    end <- indices[["end"]][i]

    all_reqs[[i]] <- httr2::req_body_form(
      req,
      !!!(compact(list(where = where, objectIds = object_ids[start:end]))),
      !!!filter_geom,
      f = "json",
      rollbackOnFailure = rollback_on_failure
    )
  }

  if (progress) {
    cli::cli_progress_done(pb)
  }

  # send the requests in parallel
  all_resps <- httr2::req_perform_parallel(all_reqs, progress = progress)

  # parse the responses into a data frame
  res <- do.call(
    rbind.data.frame,
    lapply(all_resps, function(res) {
      resp_str <- httr2::resp_body_string(res)
      catch_error(resp_str)
      resp <- RcppSimdJson::fparse(resp_str)
      resp[["deleteResults"]]
    })
  )

  data_frame(res)
}
