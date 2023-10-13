

# Add Features ------------------------------------------------------------

#' Add Features to Feature Layer
#'
#' @param x an object of class `FeatureLayer`
#' @param .data an object of class `sf` or `data.frame`
#' @param match_on whether to match on the alias or the field name. Default, the alias.
#'   See Details for more
#' @param rollback_on_fail if anything errors, roll back writes. Defaults to `TRUE`
#' @param token your authorization token. By default checks the environment variable `ARCGIS_TOKEN`
#'
#' @details
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
add_features <- function(
    x,
    .data,
    match_on = c("alias", "name"),
    rollback_on_fail = TRUE,
    token = Sys.getenv("ARCGIS_TOKEN")
) {

  stopifnot("`.data` must be a data.frame-type class" = inherits(.data, "data.frame"))

  match_on <- match.arg(match_on)

  # DEVELOPER NOTE: date fields and others should be handled
  # in the json converters in arcgisutils as_features and as_featureset

  # TODO separate into multiple different requests
  # TODO address data.frame objects / table layers
  # TODO error on list columns

  target_crs <- sf::st_crs(x)
  provided_crs <- sf::st_crs(.data)

  # see commentary in `update_features.R`
  if (!target_crs == provided_crs) {
    if (is.na(sf::st_crs(.data))) {
      warning("CRS missing from `.data` assuming ", sf::st_crs(x)$srid)
    } else if (is.na(sf::st_crs(x))) {
      warning("CRS missing from `x` cannot verify matching CRS.")
    } else {
      stop("`FeatureLayer` and `.data` have different CRS\nTranform to the same CRS:\n",
           "  `sf::st_transform(.data, sf::st_crs(x))`")
    }
  }


  # not that addFeatures does not update layer definitions so if any attributes
  # are provided that aren't in the feature layer, they will be ignored
  feature_fields <- list_fields(x)
  cnames <- colnames(.data)

  # find which columns are present in the layer
  present_index <- cnames %in% feature_fields[[match_on]]

  # fetch the geometry column name
  geo_col <- attr(.data, "sf_column")

  if (match_on == "alias") {
    lu <- setNames(feature_fields[["name"]], feature_fields[["alias"]])

    # ensure the geo_col is present if its an sf object
    if (inherits(.data, "sf")) {
      cnames[length(cnames)] <- geo_col
    } else {
      cnames <- unname(lu[cnames])
    }
    colnames(.data) <- cnames
  }

  # columns not in the feature layer
  nin_feature <- setdiff(cnames[!present_index], geo_col)

  if (length(nin_feature) > 0 ) {
    message(
      "Columns in `.data` not in feature(s): ",
      ifelse(length(nin_feature) > 1, paste0(nin_feature, collapse = ", "), nin_feature)
    )
  }

  # subset accordingly
  .data <- .data[, present_index]

  # conver to esri json
  body <- as_esri_features(.data)

  req <- httr2::request(x[["url"]])
  req <- httr2::req_url_path_append(req, "addFeatures")
  req <- httr2::req_url_query(req, token = token)

  req <- httr2::req_body_form(
    req,
    features = body,
    rollbackOnFailure = rollback_on_fail,
    f = "json"
  )

  resp <- httr2::req_perform(req)

  RcppSimdJson::fparse(
    httr2::resp_body_string(resp)
  )

  # TODO what is the behavior be after this is completed?
  # should the x object be returned? Should the successes / results be returned?
  # should the feature layer be refreshed?
}


# Update Features ---------------------------------------------------------

#' @export
#' @inheritParams add_features
#' @rdname modify
update_features <- function(
    x,
    .data,
    match_on = c("alias", "name"),
    token = Sys.getenv("ARCGIS_TOKEN"),
    rollback_on_failure = TRUE,
    ...
) {

  match_on <- match.arg(match_on)

  # TODO field types from x need to be compared to that of `.data`
  # if the field types do not match either error or do the conversion for the user

  # What is the difference between `applyEdits`, and `append` both are a way to do upserting.

  # Update Feature Layer
  # https://developers.arcgis.com/rest/services-reference/enterprise/update-features.htm

  #  check CRS compaitibility between x and `.data`
  # feedback for rest api team:
  # it is possible to specify the CRS of the input data for adding features or spatial
  # filters, however it is _not_ possible for updating features. If it is, it is undocumented
  # it would be nice to be able to utilize the the transformations server side rather than
  # relying on GDAL client side.
  # ALTERNATIVELY let me provide a feature set so i can pass in CRS
  if (!identical(sf::st_crs(x), sf::st_crs(.data))) {

    if (is.na(sf::st_crs(.data)) && inherits(.data, "sf")) {
      warning("CRS missing from `.data` assuming ", sf::st_crs(x)$srid)
    } else if (inherits(.data, "sf")){
      stop("`FeatureLayer` and `.data` have different CRS\nTranform to the same CRS:\n",
           "  `sf::st_transform(.data, sf::st_crs(x))`")
    }
  }  # not that addFeatures does not update layer definitions so if any attributes
  # are provided that aren't in the feature layer, they will be ignored
  feature_fields <- list_fields(x)
  cnames <- colnames(.data)

  # find which columns are present in the layer
  present_index <- cnames %in% feature_fields[[match_on]]

  # fetch the geometry column name
  geo_col <- attr(.data, "sf_column")

  if (match_on == "alias") {
    lu <- setNames(feature_fields[["name"]], feature_fields[["alias"]])

    # ensure the geo_col is present if its an sf object
    if (inherits(.data, "sf")) {
      cnames[length(cnames)] <- geo_col
    } else {
      cnames <- unname(lu[cnames])
    }
    colnames(.data) <- cnames
  }

  # columns not in the feature layer
  nin_feature <- setdiff(cnames[!present_index], geo_col)

  if (length(nin_feature) > 0 ) {
    message(
      "Columns in `.data` not in feature(s): ",
      ifelse(length(nin_feature) > 1, paste0(nin_feature, collapse = ", "), nin_feature)
    )
  }

  # subset accordingly
  .data <- .data[, present_index]

  # create base request
  req <- httr2::request(
    paste0(x[["url"]], "/updateFeatures")
  )

  req <- httr2::req_body_form(
    req,
    # transform `.data`
    features = as_esri_features(.data),
    rollbackOnFailure = rollback_on_failure,
    token = token,
    f = "json",
    ...
  )

  resp <- httr2::req_perform(req)
  jsonify::from_json(httr2::resp_body_string(resp))
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
#' @param rollback_on_fail default `TRUE`. Specifies whether the edits should be
#'   applied only if all submitted edits succeed.
#' @param token your authorization token. By default, token is set to the
#'   environment variable `ARCGIS_TOKEN`. Use `set_auth_token()` to set
#'   `ARCGIS_TOKEN`.
#' @export
#' @rdname modify
delete_features <- function(x,
                            object_ids = NULL,
                            where = NULL,
                            filter_geom = NULL,
                            predicate = "intersects",
                            rollback_on_fail = TRUE,
                            token = Sys.getenv("ARCGIS_TOKEN"),
                            ...) {

  obj_check_layer(x)

  # where, oid, or filter_geom necessary
  if (is.null(object_ids) && is.null(where) && is.null(filter_geom)) {
    cli_abort(
      c("No features to delete",
      "i" = "Supply at least one of {.arg object_ids}, {.arg where},
      or {.arg filter_geom}")
    )
  }

  if (!is.null(object_ids)) {
    object_ids <- paste0(object_ids, collapse = ",")
  }

  filter_geom <- filter_geom %||% list()

  # convert to the proper CRS if not missing
  if (!rlang::is_empty(filter_geom)) {
    filter_geom <- prepare_spatial_filter(
      filter_geom = filter_geom,
      predicate = predicate,
      crs = sf::crs(x)
    )
  }

  # https://developers.arcgis.com/rest/services-reference/enterprise/delete-features.htm
  req <- httr2::request(paste0(x[["url"]], "/deleteFeatures"))

  req <- httr2::req_body_form(
    req,
    !!!(compact(list(where = where, objectIds = object_ids))),
    !!!filter_geom,
    f = "json",
    token = token,
    rollbackOnFailure = rollback_on_fail,
    ...
  )

  resp <- httr2::req_perform(req)

  jsonify::from_json(httr2::resp_body_string(resp))
}



