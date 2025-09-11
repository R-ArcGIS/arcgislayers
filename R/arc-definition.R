#' Add, update, or delete a Feature Layer definition
#'
#' Each layer of a feature service is defined by a "definition." The definition
#' describes the service such as its fields, symbology, indexes and more.
#'
#' @details
#'
#'  `r lifecycle::badge("experimental")`
#'
#' - Use [add_layer_definition()] for adding fields to a feature service or otherwise
#' adding to the definition of a feature layer.
#' - Use [update_layer_definition()] to modify existing aspects of the definition properties.
#' - Use [delete_layer_definition()] to delete properties from the layer definition.
#'
#' Examples of properties include the layer name, renderer, or field properties. Named parameters
#' passed to `...` must have names matching supported definitions.

#' Parameters are converted to a JSON `addToDefinition`, `updateDefinition`, or
#' `deleteFromDefinition` query parameter using [jsonify::to_json()].
#'
#' See the ArcGIS REST API documentation on Administer Hosted Feature Services
#' for more details:
#'
#' - see the [layerDefinition](https://developers.arcgis.com/web-map-specification/objects/layerDefinition/) object documentation.
#' - adding definitions for a [FeatureLayer](https://developers.arcgis.com/rest/services-reference/online/add-to-definition-feature-layer/) or [a FeatureService](https://developers.arcgis.com/rest/services-reference/online/add-to-definition-feature-service/)
#' - updating definitions for [a
#' FeatureLayer](https://developers.arcgis.com/rest/services-reference/online/update-definition-feature-layer/) or [a
#' FeatureService](https://developers.arcgis.com/rest/services-reference/online/update-definition-feature-service-.htm)
#' - deleting definitions for [a FeatureLayer](https://developers.arcgis.com/rest/services-reference/online/delete-from-definition-feature-layer/) or a [FeatureService](https://developers.arcgis.com/rest/services-reference/online/delete-from-definition-feature-service/)
#'
#' @param x A Feature Layer, Table, or Feature Service class object.
#' @param ... Additional parameters for the "addToDefinition" or "updateDefinition" body of the request.
#' @param async Default `FALSE`. If `TRUE`, support asynchronous processing for
#'   the request.
#' @inheritParams arc_open
#' @returns If `async = FALSE`, return an updated "FeatureServer" or "FeatureLayer" object with the added, updated, or deleted definitions. If `async = TRUE`, the input Feature Layer or Feature Server object `x` is returned as is.
#' @rdname definition
#' @export
#' @examples
#' \dontrun{
#' if (interactive()) {
#' # authenticate
#' set_arc_token(auth_code())
#'
#' # publish a layer
#' published <- publish_layer(penguins, "Penguin Test")
#'
#' penguin_fl <- arc_open(published$services$encodedServiceURL) |>
#'   get_layer(0)
#'
#' # Update the name of the layer
#' update_layer_definition(
#'   penguin_fl,
#'   name = "New Layer Name"
#' )
#'
#' # add an index on the the layer
#' add_layer_definition(
#'   penguin_fl,
#'   indexes = list(
#'     name = "index1",
#'     fields = "species",
#'     isUnique = FALSE,
#'     isAscending = FALSE,
#'     description = "Example index"
#'   )
#' )
#'
#' # refresh the layer to get the updates
#' penguin_fl <- refresh_layer(penguin_fl)
#' penguin_fl[["indexes"]]
#' }
#' }
add_layer_definition <- function(
  x,
  ...,
  async = FALSE,
  token = arc_token()
) {
  check_inherits_any(x, c("FeatureServer", "FeatureLayer", "Table"))

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

  resp <- httr2::req_perform(req)
  check_resp_body_error(resp = resp)

  if (!async) {
    # Refresh x to include updated definitions
    x <- arc_open(x[["url"]], token = token)
  }

  invisible(x)
}

#' @rdname definition
#' @export
update_layer_definition <- function(
  x,
  ...,
  async = FALSE,
  token = arc_token()
) {
  check_inherits_any(x, c("FeatureServer", "FeatureLayer", "Table"))

  req <- arc_base_req(
    url = as_admin_service_url(x[["url"]]),
    token = token,
    path = "updateDefinition"
  )

  update_definition <- rlang::list2(...)
  check_dots_named(update_definition)

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

  if (!async) {
    # Refresh x to include updated definitions
    x <- arc_open(x[["url"]], token = token)
  }

  x
}

#' @rdname definition
#' @export
delete_layer_definition <- function(
  x,
  ...,
  async = FALSE,
  token = arc_token()
) {
  check_inherits_any(x, c("FeatureServer", "FeatureLayer", "Table"))

  req <- arc_base_req(
    url = as_admin_service_url(x[["url"]]),
    token = token,
    path = "deleteFromDefinition"
  )

  delete_definition <- rlang::list2(...)
  check_dots_named(delete_definition)

  req <- httr2::req_body_form(
    req,
    deleteFromDefinition = jsonify::to_json(
      delete_definition,
      unbox = TRUE
    ),
    async = async,
    f = "json"
  )

  resp <- httr2::req_perform(req)
  check_resp_body_error(resp = resp)

  if (!async) {
    # Refresh x to include deleted definitions
    x <- arc_open(x[["url"]], token = token)
  }

  x
}

#' Print existing and (optionally) updated definition values
#' @noRd
print_definition_values <- function(
  existing,
  updated = NULL,
  what = "Feature Layer",
  action = "Updated"
) {
  cli::cli_inform(
    c(
      "Set the {.field cliExtras.quiet} option to stop printing definition values.",
      "*" = "Use {.code options(cli.default_handler = suppressMessages)} to set the option."
    ),
    .frequency = "once",
    .frequency_id = "print_definition_values"
  )

  # Customize theme for messages
  dl_theme <- cli::cli_div(
    theme = list(
      dl = list(`list-style-type` = cli::symbol$bullet),
      span.dt = list(before = "`", after = "`"),
      span.dd = list(color = "blue")
    )
  )

  cli::cli_rule(
    "{cli::symbol$tick} {action} {length(existing)} {what} definition{?s}."
  )

  if (is.null(updated)) {
    lapply(
      seq_along(existing),
      function(x) {
        cli::cli_bullets(
          c(" " = "{.dt {names(existing)[x]}} {.dd {existing[x]}}")
        )
      }
    )
  } else {
    # Fill blank values for missing names in existing
    diff_nm <- is.element(names(updated), names(existing))
    if (!all(diff_nm)) {
      existing[!diff_nm] <- rep(" ", sum(!diff_nm))
      names(existing) <- names(updated)
    }

    lapply(
      seq_along(existing),
      function(x) {
        cli::cli_bullets(
          c(
            " " = "{.dt {names(existing)[x]}} {.dd {existing[x]}} {cli::symbol$arrow_right} {.dd {updated[x]}}"
          )
        )
      }
    )
  }

  cli::cli_end(dl_theme)
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
  error_call = rlang::caller_env()
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
