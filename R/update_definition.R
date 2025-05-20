#' Update a Feature Layer or Feature Service definition
#'
#' [update_definition()] supports updating a definition property for a hosted
#' Feature Service or Feature Layer. Examples of properties include the layer
#' name, renderer, or field properties. Parameters passed to `...` must have
#' names matching the definitions. Parameters are converted to a JSON
#' `updateDefinition` parameter using [jsonify::to_json()]. See the ArcGIS REST API documentation on Administer Hosted Feature Services
#' for more details on updating the definition for [a
#' FeatureService](https://developers.arcgis.com/rest/services-reference/online/update-definition-feature-service-.htm)
#' or [a
#' FeatureLayer](https://developers.arcgis.com/rest/services-reference/online/update-definition-feature-layer/).
#'
#' @param x A Feature Layer or Feature Service class object.
#' @param ... Additional parameters "updateDefinition" body for the request.
#' @param async Default `FALSE`. If `TRUE`, support asynchronous processing for
#'   the request.
#' @inheritParams arc_open
#' @export
update_definition <- function(
  x = NULL,
  ...,
  async = FALSE,
  token = arc_token()
) {
  check_inherits_any(x, c("FeatureServer", "FeatureLayer"))

  req <- arc_base_req(
    # Modify URL to be `adminservicecatalog-url`
    # See <https://developers.arcgis.com/rest/services-reference/online/>
    url = sub(
      "rest/services",
      "rest/admin/services",
      x[["url"]]
    ),
    token = token,
    path = "updateDefinition"
  )

  # TODO: Add check for existence of definition properties
  # See https://developers.arcgis.com/rest/services-reference/online/add-to-definition-feature-layer/
  update_definition <- rlang::list2(...)

  # Customize theme for messages
  dl_theme <- cli::cli_div(
    theme = list(
      span.dt = list(before = "`", after = "`"),
      span.dd = list(color = "blue")
    )
  )

  # Pull existing service/layer definition values
  definition <- x[names(x) %in% names(update_definition)]

  # Display existing service/layer definition values
  if (length(definition) > 0 && any(definition != "")) {
    cli::cli_bullets(c("i" = "Existing definition values:"))
    cli::cli_dl(items = definition)
  }

  req <- httr2::req_body_form(
    req,
    updateDefinition = jsonify::to_json(
      update_definition,
      unbox = TRUE
    ),
    async = async,
    f = "json"
  )

  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp)

  # Handle error messages
  if (rlang::has_name(body, "error")) {
    message <- body[["error"]][["message"]]
    details <- unlist(body[["error"]][["details"]])
    if (message != details) {
      message <- c(message, details)
    }

    cli::cli_abort(message)
  }

  # Display update
  cli::cli_bullets(c("v" = "Updated definition values:"))
  cli::cli_dl(items = update_definition)
  cli::cli_end(dl_theme)

  # TODO: Explore if x should be refreshed w/ updated definition via a call to
  # arc_open
  invisible(x)
}
