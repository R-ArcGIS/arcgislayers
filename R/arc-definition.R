#' Add, update, or delete a Feature Layer or Feature Service definition
#'
#' [add_definition()] and [update_definition()] support adding or updating
#' definition properties for a hosted Feature Service or Feature Layer.
#' [delete_definition()] supports deleting a definition. Examples of properties
#' include the layer name, renderer, or field properties. Parameters passed to
#' `...` must have names matching the definitions. Parameters are converted to a
#' JSON `addToDefinition` or `updateDefinition` parameter using
#' [jsonify::to_json()].
#'
#' See the ArcGIS REST API documentation on Administer Hosted Feature Services
#' for more details:
#'
#' - adding definitions for a [FeatureLayer](https://developers.arcgis.com/rest/services-reference/online/add-to-definition-feature-layer/) or [a FeatureService](https://developers.arcgis.com/rest/services-reference/online/add-to-definition-feature-service/)
#' - updating definitions for [a
#' FeatureLayer](https://developers.arcgis.com/rest/services-reference/online/update-definition-feature-layer/) or [a
#' FeatureService](https://developers.arcgis.com/rest/services-reference/online/update-definition-feature-service-.htm)
#' - deleting definitions for [a FeatureLayer](https://developers.arcgis.com/rest/services-reference/online/delete-from-definition-feature-layer/) or a [FeatureService](https://developers.arcgis.com/rest/services-reference/online/delete-from-definition-feature-service/)
#'
#' @param x A Feature Layer or Feature Service class object.
#' @param ... Additional parameters for the "addToDefinition" or "updateDefinition" body of the request.
#' @param async Default `FALSE`. If `TRUE`, support asynchronous processing for
#'   the request.
#' @inheritParams arc_open
#' @returns An updated "FeatureServer" or "FeatureLayer" object.
#' @rdname definition
#' @export
add_definition <- function(
  x,
  ...,
  async = FALSE,
  token = arc_token()
) {
  check_inherits_any(x, c("FeatureServer", "FeatureLayer"))

  req <- arc_base_req(
    url = as_admin_service_url(x[["url"]]),
    token = token,
    path = "addToDefinition"
  )

  add_definition <- rlang::list2(...)

  req <- httr2::req_body_form(
    req,
    addToDefinition = jsonify::to_json(
      add_definition,
      unbox = TRUE
    ),
    async = async,
    f = "json"
  )

  # TODO: Consider adding message showing added definitions

  resp <- httr2::req_perform(req)
  check_resp_body_error(resp = resp)

  # Refresh x to include added definitions
  x <- arc_open(x[["url"]], token = token)
  invisible(x)
}

#' @rdname definition
#' @export
update_definition <- function(
  x,
  ...,
  async = FALSE,
  token = arc_token(),
  verbose = TRUE
) {
  check_inherits_any(x, c("FeatureServer", "FeatureLayer"))

  req <- arc_base_req(
    url = as_admin_service_url(x[["url"]]),
    token = token,
    path = "updateDefinition"
  )

  update_definition <- rlang::list2(...)
  arcgisutils::check_dots_names(update_definition)

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
  if (length(definition) > 0 && any(definition != "") && verbose) {
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
  check_resp_body_error(resp = resp)

  # Display update
  if (verbose) {
    cli::cli_bullets(c("v" = "Updated definition values:"))
    cli::cli_dl(items = update_definition)
    cli::cli_end(dl_theme)
   }

  # Refresh x to include updated definitions
  x <- arc_open(x[["url"]], token = token)
  invisible(x)
}

#' @rdname definition
#' @export
delete_definition <- function(
  x,
  ...,
  async = FALSE,
  token = arc_token()
) {
  check_inherits_any(x, c("FeatureServer", "FeatureLayer"))

  req <- arc_base_req(
    url = as_admin_service_url(x[["url"]]),
    token = token,
    path = "deleteFromDefinition"
  )

  delete_definition <- rlang::list2(...)

  req <- httr2::req_body_form(
    req,
    deleteFromDefinition = jsonify::to_json(
      delete_definition,
      unbox = TRUE
    ),
    async = async,
    f = "json"
  )

  # TODO: Consider adding message showing deleted definitions

  resp <- httr2::req_perform(req)
  check_resp_body_error(resp = resp)

  # Refresh x to include added definitions
  x <- arc_open(x[["url"]], token = token)
  invisible(x)
}

#' Convert object URL to adminservicecatalog url
#' See <https://developers.arcgis.com/rest/services-reference/online/>
#' @noRd
as_admin_service_url <- function(url) {
  sub("rest/services", "rest/admin/services", url)
}

#' Check a response body for an error message
#' @noRd
check_resp_body_error <- function(
  resp,
  error_call = caller_env()
) {
  body <- httr2::resp_body_json(resp)

  # Return if no error
  if (!rlang::has_name(body, "error")) {
    return(invisible(NULL))
  }

  # Pull error message from body
  cli::cli_abort(
    unlist(body[["error"]][["details"]]),
    call = error_call
  )
}
