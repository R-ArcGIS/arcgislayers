

# Add Features ------------------------------------------------------------

#' Add Features to Feature Layer
#'
#' @param x an object of class `FeatureLayer`
#' @param .data an object of class `sf` or `data.frame`
#' @param rollback_on_fail if anything errors, roll back writes. Defaults to `TRUE`
#' @param token your authorization token. By default checks the environment variable `ARCGIS_TOKEN`
#'
#' @export
#' @rdname modify
add_features <- function(
    x,
    .data,
    rollback_on_fail = TRUE,
    token = Sys.getenv("ARCGIS_TOKEN")
) {

  # TODO separate into multiple different requests
  # TODO manipulate date types before sending up
  # TODO address data.frame objects / table layers
  # TODO error on list columns

  target_crs <- sf::st_crs(x)
  provided_crs <- sf::st_crs(.data)

  # see commentary in `update_features.R`
  if (!identical(sf::st_crs(x), sf::st_crs(.data))) {
    if (is.na(sf::st_crs(.data))) {
      warning("CRS missing from `.data` assuming ", sf::st_crs(x)$srid)
    } else {
      stop("`FeatureLayer` and `.data` have different CRS\nTranform to the same CRS:\n",
           "  `sf::st_transform(.data, sf::st_crs(x))`")
    }
  }


  # not that addFeatures does not update layer definitions so if any attributes
  # are provided that aren't in the feature layer, they will be ignored

  feature_fields <- list_fields(x)

  cnames <- colnames(.data)

  present_index <- cnames %in% feature_fields[["name"]]

  geo_col <- attr(.data, "sf_column")

  # columns not in the feature layer
  nin_feature <- setdiff(cnames[!present_index], geo_col)

  if (length(nin_feature) > 0 ) {
    message(
      "Columns in `.data` not in feature(s): ",
      ifelse(length(nin_feature) > 1, paste0(nin_feature, collapse = ", "), nin_feature)
    )

    # Prompt if we want to have it be a bit more interactive
    # if (interactive()) {
    #   prmpt <- readline("Do you wish to continue? (y/n): ")
    #
    #   while(!prmpt %in% c("y", "n")) {
    #     message(r"{Please enter "y" or "n":}")
    #     prmpt <- readline("Do you wish to continue? (y/n): ")
    #
    #     if (prmpt == "y") stop("Stopping...", call. = FALSE)
    #   }
    # }
  }


  .data <- .data[, present_index]
  body <- st_as_features(.data)

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
  #
  # # should the feature layer be refreshed?
  # flayer2 <- feature_layer("https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/ArcGIS/rest/services/polygon_testing/FeatureServer/0", token = token)
  #

}



# Update Features ---------------------------------------------------------

#' @export
#' @rdname modify
update_features <- function(
    x,
    .data,
    token = Sys.getenv("ARCGIS_TOKEN"),
    rollback_on_failure = TRUE,
    ...
) {

  # TODO field types from x need to be compared to that of `.data`
  # if the field types do not match either error or do the conversion for the user

  # What is the difference between `applyEdits`, and `append` both are a way to do upserting.

  # Update Feature Layer
  #
  #
  #
  # https://developers.arcgis.com/rest/services-reference/enterprise/update-features.htm

  #  check CRS compaitibility between x and `.data`
  # feedback for rest api team:
  # it is possible to specify the CRS of the input data for adding features or spatial
  # filters, however it is _not_ possible for updating features. If it is, it is undocumented
  # it would be nice to be able to utilize the the transformations server side rather than
  # relying on GDAL client side.
  # ALTERNATIVELY let me provide a feature set so i can pass in CRS
  if (!identical(sf::st_crs(x), sf::st_crs(.data))) {

    if (is.na(sf::st_crs(.data))) {
      warning("CRS missing from `.data` assuming ", sf::st_crs(x)$srid)
    } else {
      stop("`FeatureLayer` and `.data` have different CRS\nTranform to the same CRS:\n",
           "  `sf::st_transform(.data, sf::st_crs(x))`")
    }
  }

  # check for field compatibility. Warn and drop fields if they aren't present.
  # get field names from feature layer
  cur_fields <- tolower(list_fields(x)[["name"]])

  # check provided columns
  data_cols <- tolower(colnames(.data))

  # drop geometry columns from the vector
  data_cols <- data_cols[which(!data_cols == attr(.data, "sf_column"))]

  # identify which columns are not in `x`
  nindex <- !(data_cols %in% cur_fields)

  # stop processing
  if (any(nindex)) {
    stop("Fields in `.data` not present in `x`\n",
         "\nVars: ", paste0("`", data_cols[nindex], "`", collapse = ", "))
  }

  # create base request
  req <- httr2::request(
    paste0(x[["url"]], "/updateFeatures")
  )

  req <- httr2::req_body_form(
    req,
    # transform `.data`
    features = st_as_features(.data),
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
#' Delete features from a feature layer based on object ID, a where clause, or a spatial filter.
#'
#' @param x a `FeatureLayer`
#' @param object_ids a numeric vector of object IDs to be deleted.
#' @param where a simple SQL where statement indicating which features should be deleted.
#'  When the where statement evaluates to `TRUE`, those values will be deleted.
#' @param filter_geom an `sfc` or `sfg` object. If `sfc` its length must be one.
#' @param predicate deault `"intersects"`. The spatial predicate to be used for the spatial filter. Ignored if `filter_geom` is not provided.
#' @param rollback_on_fail default `TRUE`. Specifies whether the edits should be applied only if all submitted edits succeed.
#' @param token your authorization token. By default uses the environment variable `ARCGIS_TOKEN` as set by `set_auth_token()`.
#' @export
#' @rdname modify
delete_features <- function(x,
                            object_ids,
                            where,
                            filter_geom,
                            predicate = "intersects",
                            rollback_on_fail = TRUE,
                            token = Sys.getenv("ARCGIS_TOKEN"),
                            ...) {

  # where, oid, or filter_geom necessary
  if (missing(object_ids) && missing(where) && missing(filter_geom)) {
    stop(
      "No features to delete\n  Supply at least one of the following arguments
    - `object_ids`, `where`, or `filter_geom`"
    )
  }

  if (!missing(object_ids)) {
    object_ids <- paste0(object_ids, collapse = ",")
  } else {
    object_ids <- NULL
  }

  # if where is missing set to NULL so it can be easily removed via `compact()`
  if (missing(where)) where <- NULL

  # convert to the proper CRS if not missing
  if (!missing(filter_geom)) {
    filter_geom <- prepare_spatial_filter(x, filter_geom, predicate)
  } else {
    filter_geom <- list()
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



