# TODO separate into multiple different requests
# TODO manipulate date types before sending up
# TODO address data.frame objects / table layers
# TODO error on list columns

#' Add Features to Feature Layer
#'
#' @param feature an object of class `FeatureLayer`
#' @param .data an object of class `sf` or `data.frame`
#' @param rollback_on_fail if anything errors, roll back writes. Defaults to `TRUE`
#' @param token your authentication token. By default checks the environment variable `ARCGIS_TOKEN`
#' @export
add_features <- function(feature,
                            .data,
                            rollback_on_fail = TRUE,
                            token = Sys.getenv("ARCGIS_TOKEN")) {


  target_crs <- st_crs(feature)
  provided_crs <- sf::st_crs(.data)

  if (!identical(target_crs, provided_crs)) {

    warning("CRS do not match")
    message(glue::glue(
      "Transforming `.data` from {provided_crs$input} to {target_crs$input}"
      ))
    .data <- st_transform(.data, target_crs)
  }

  # TODO check CRS of the feature layer vs the data provided
  # will have to perform transformation if incorrect or provide error
  # either or

  # not that addFeatures does not update layer definitions so if any attributes
  # are provided that aren't in the feature layer, they will be ignored

  # TODO assumes that the names of the features are identical

  feature_fields <- list_fields(feature)

  cnames <- colnames(.data)

  present_index <- cnames %in% feature_fields[["name"]]

  geo_col <- attr(.data, "sf_column")

  # columns not in the feature layer
  nin_feature <- setdiff(cnames[!present_index], geo_col)

  if (length(nin_feature) > 0 ) warning(
    "Columns in `.data` not in feature(s): ",
    ifelse(length(nin_feature) > 1, paste0(nin_feature, collapse = ", "), nin_feature)
  )

  .data <- .data[, present_index]
  body <- st_as_features(.data)

  req <- httr2::request(feature[["url"]])
  req <- httr2::req_url_path_append(req, "addFeatures")
  req <- httr2::req_url_query(req, token = token)

  req <- httr2::req_body_form(
    req,
    features = body,
    rollbackOnFailure = rollback_on_fail,
    f = "json"
  )

  resp <- httr2::req_perform(req)

  resp |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse()

}
#
# # should the feature layer be refreshed?
# flayer2 <- feature_layer("https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/ArcGIS/rest/services/polygon_testing/FeatureServer/0", token = token)
#
