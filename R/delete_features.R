

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


