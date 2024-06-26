#' Update Feature Layer or Service definition
#'
#' <https://developers.arcgis.com/rest/services-reference/online/update-definition-feature-layer-.htm#ESRI_SECTION2_027B8A04953C409E9A9D9051DD4CDBEE>
#' <https://developers.arcgis.com/rest/services-reference/online/update-definition-feature-service-.htm>
#'
#' @param x A Feature Layer or Feature Service class object.
#' @param update A named list used as the "updateDefinition" body for the
#'   request.
#' @param async Default `FALSE`. If `TRUE`, support asynchronous processing for
#'   the request.
#' @export
update_definition <- function(
    x,
    update = NULL,
    async = FALSE,
    token = arc_token()) {
  check_inherits_any(x, c("FeatureServer", "FeatureLayer"))

  req <- arc_base_req(
    url = x[["url"]],
    token = token,
    path = "updateDefinition"
  )

  req <- httr2::req_body_json(
    req,
    list(
      updateDefinition = update,
      async = tolower(async),
      f = "json"
    )
  )

  resp <- httr2::req_perform(req)

  resp
}

#' @rdname update_definition
#' @export
.update_definition_name <- function(...) {
  rlang::list2(...)
}

#' @rdname update_definition
#' @export
.update_layer_properties <- function(
    description = NULL,
    license_text = NULL,
    xss_trusted_fields = NULL,
    display_field = NULL,
    min_scale = NULL,
    max_scale = NULL,
    max_records = NULL,
    default_visibility = NULL,
    ...) {
  check_string(description, allow_null = TRUE)
  check_string(license_text, allow_null = TRUE)

  if (is.character(xss_trusted_fields) && length(xss_trusted_fields) > 1) {
    xss_trusted_fields <- paste0(xss_trusted_fields, collapse = ", ")
  }

  check_string(xss_trusted_fields, allow_null = TRUE)

  # FIXME: Avoid using internal arcgisutils functions
  arcgisutils:::check_number_whole(min_scale, allow_null = TRUE)
  arcgisutils:::check_number_whole(max_scale, allow_null = TRUE)
  arcgisutils:::check_number_whole(max_records, allow_null = TRUE)
  arcgisutils:::check_logical(default_visibility, allow_null = TRUE)

  compact(
    list(
      xssTrustedFields = xss_trusted_fields,
      displayField = display_field,
      description = description,
      copywriteText = license_text,
      minScale = min_scale,
      maxScale = max_scale,
      maxRecordCount = max_records,
      # standardMaxRecordCount = max_records,
      # tileMaxRecordCount = max_records,
      # maxRecordCountFactor = 1,
      # types = "",
      # templates = "",
      defaultVisibility = tolower(default_visibility)
      # typeIdField = ""
    )
  )
}

.update_service_properties <- function(...) {
  # TODO: Add service property definition

  # {
  #   "name": "SanFrancisco2",
  #   "hasStaticData": false,
  #   "allowGeometryUpdates": "true",
  #   "maxRecordCount": 2000,
  #   "serviceDescription": "SanFrancisco2",
  #   "description": "",
  #   "copyrightText": "",
  #   "units": "",
  #   "xssPreventionInfo": {
  #     "xssPreventionEnabled": true,
  #     "xssPreventionRule": "InputOnly",
  #     "xssInputRule": "rejectInvalid"
  #   },
  #   "initialExtent": {
  #     "xmin": -122.514435102,
  #     "ymin": 5.6843418860808E-14,
  #     "xmax": 138.625776397,
  #     "ymax": 67.1577965990001
  #   }
  # }
  #
}
