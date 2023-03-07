
# TODO separate into multiple different requests
# TODO manipulate date types before sending up
# TODO address data.frame objects / table layers
# TODO error on list columns

#' Add Features to Feature Layer
#'
#' @param x an object of class `FeatureLayer`
#' @param .data an object of class `sf` or `data.frame`
#' @param rollback_on_fail if anything errors, roll back writes. Defaults to `TRUE`
#' @param token your authorization token. By default checks the environment variable `ARCGIS_TOKEN`
#' @export
add_features <- function(
    x,
    .data,
    rollback_on_fail = TRUE,
    token = Sys.getenv("ARCGIS_TOKEN")
) {


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

  resp |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse()

  # TODO what is the behavior be after this is completed?
  # should the x object be returned? Should the successes / results be returned?

}
#
# # should the feature layer be refreshed?
# flayer2 <- feature_layer("https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/ArcGIS/rest/services/polygon_testing/FeatureServer/0", token = token)
#
