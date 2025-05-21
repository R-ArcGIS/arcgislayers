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
#' @param token your authorization token.
#'
#' @inheritParams arc_select
#' @details
#'
#' `r lifecycle::badge("experimental")`
#'
#' For a more detailed guide to adding, updating, and deleting features, view the
#' tutorial on the [R-ArcGIS Bridge website](https://developers.arcgis.com/r-bridge/editing/editing-overview/).
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
  chunk_size = 2000,
  match_on = c("name", "alias"),
  rollback_on_failure = TRUE,
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
  .data <- .data[, present_index]

  # count the number of rows in a data frame
  n <- nrow(.data)

  # get the indices to chunk them up
  indices <- chunk_indices(n, chunk_size)

  # create the base request from the feature url
  base_req <- arc_base_req(x[["url"]], token)
  req <- httr2::req_url_path_append(base_req, "addFeatures")

  # pre-allocate list
  all_reqs <- vector("list", length = lengths(indices)[1])

  # populate the requests veector
  for (i in seq_along(all_reqs)) {
    start <- indices[["start"]][i]
    end <- indices[["end"]][i]

    all_reqs[[i]] <- httr2::req_body_form(
      req,
      features = as_esri_features(.data[start:end, ]),
      rollbackOnFailure = rollback_on_failure,
      f = "json"
    )
  }

  # send the requests in parallel
  all_resps <- httr2::req_perform_parallel(all_reqs)

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


# Update Features ---------------------------------------------------------

#' @export
#' @inheritParams add_features
#' @rdname modify
update_features <- function(
  x,
  .data,
  match_on = c("name", "alias"),
  token = arc_token(),
  rollback_on_failure = TRUE,
  ...
) {
  match_on <- match.arg(match_on)

  # TODO field types from x need to be compared to that of `.data`
  # if the field types do not match either error or do the conversion for the user

  # What is the difference between `applyEdits`, and `append` both are a way to do upserting.

  # Update Feature Layer
  # https://developers.arcgis.com/rest/services-reference/enterprise/update-features.htm
  # check CRS compaitibility between x and `.data`
  # feedback for rest api team:
  # it is possible to specify the CRS of the input data for adding features or spatial
  # filters, however it is _not_ possible for updating features. If it is, it is undocumented
  # it would be nice to be able to utilize the the transformations server side
  # rather than relying on GDAL client side.
  # ALTERNATIVELY let me provide a feature set so i can pass in CRS
  if (!identical(sf::st_crs(x), sf::st_crs(.data))) {
    if (is.na(sf::st_crs(.data)) && inherits(.data, "sf")) {
      cli::cli_warn(
        c(
          "{.arg data} is missing a CRS",
          "i" = paste0("Setting CRS to ", sf::st_crs(x)$srid)
        )
      )
    } else if (inherits(.data, "sf")) {
      cli::cli_abort(
        c(
          "{.arg x} and {.arg .data} must share the same CRS",
          "*" = "Tranform {.arg .data} to the same CRS as {.arg x} with
          {.fn sf::st_transform}"
        )
      )
    }
  } # not that addFeatures does not update layer definitions so if any attributes
  # are provided that aren't in the feature layer, they will be ignored

  objectid_condition <- is.integer(.data[["objectid"]])
  if (!objectid_condition) {
  cli::cli_abort(
    c(
      "x" = "The {.field objectid} column must be of type {.cls integer}.",
      "i" = "Convert with {.code as.integer()} if needed."
    )
  )
  }

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
  .data <- .data[, present_index]

  # create base request
  req <- arc_base_req(paste0(x[["url"]], "/updateFeatures"), token)

  req <- httr2::req_body_form(
    req,
    # transform `.data`
    features = as_esri_features(.data),
    rollbackOnFailure = rollback_on_failure,
    f = "json",
    ...
  )

  resp <- httr2::req_perform(req)
  RcppSimdJson::fparse(httr2::resp_body_string(resp))
}

#' @noRd
inform_nin_feature <- function(nin_feature) {
  if (length(nin_feature) == 0) {
    return(invisible(NULL))
  }

  cli::cli_inform(
    paste0(
      "Columns in `.data` not in feature(s): ",
      ifelse(
        length(nin_feature) > 1,
        paste0(nin_feature, collapse = ", "),
        nin_feature
      )
    )
  )
}


# Delete Features ---------------------------------------------------------

#' Delete Features from Feature Layer
#'
#' Delete features from a feature layer based on object ID, a where clause, or a
#' spatial filter.
#'
#' @inheritParams obj_check_layer
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
  token = arc_token(),
  ...
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

  req <- httr2::req_body_form(
    req,
    !!!(compact(list(where = where, objectIds = object_ids))),
    !!!filter_geom,
    f = "json",
    rollbackOnFailure = rollback_on_failure,
    ...
  )

  resp <- httr2::req_perform(req)

  RcppSimdJson::fparse(httr2::resp_body_string(resp))
}
