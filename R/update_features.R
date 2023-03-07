# TODO field types from x need to be compared to that of `.data`
# if the field types do not match either error or do the conversion for the user

# What is the difference between `applyEdits`, and `append` both are a way to do upserting.

#' Update Feature Layer
#'
#' @export
#'
update_features <- function(
    x,
    .data,
    token = Sys.getenv("ARCGIS_TOKEN"),
    rollback_on_failure = TRUE,
    ...
) {

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
