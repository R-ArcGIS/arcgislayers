#' Truncate a Feature Layer
#'
#' Removes all features in a Feature Layer or Table and resets the object ID
#' counter. Truncating a Feature Layer does not change the schema of the data
#' (does not add, remove, or alter existing database columns, constraints,
#' or indexes).
#'
#' @inheritParams arc_select
#' @param async default `FALSE`. It is recommended to set `TRUE` for
#'    larger datasets.
#' @param attachment_only default `FALSE`. Deletes all the attachments for this
#'    layer. None of the layer features will be deleted when `TRUE`.
#' @references [ArcGIS Developers Rest API Doc](https://developers.arcgis.com/rest/services-reference/online/truncate-feature-layer-.htm)
#'
#' @returns a named list with the name "success" and a value of `TRUE` or `FALSE`
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'
#'   # authorize using code flow
#'   set_auth_token(auth_code())
#'
#'   # create a FeatureLayer object
#'   flayer <- arc_open("your-feature-layer-url")
#'
#'   # truncate it
#'   truncate_layer(flayer)
#' }
#' }
#' @export
truncate_layer <- function(
    x,
    async = FALSE,
    attachment_only = FALSE,
    token = arc_token()
) {

  # check to see if it is a compatible class
  obj_check_layer(x)

  # ensure that it supports truncate
  if (!x[["supportsTruncate"]]) {
    obj <- rlang::caller_arg(x)
    cli::cli_abort(
      "{.arg {obj}} does not support the {.code truncate } operation"
    )
  }

  # extract url and its components
  furl <- x[["url"]]
  parts <- httr2::url_parse(furl)

  # replace /services/ with /admin/services/
  parts[["path"]] <- sub("\\/services/", "/admin/services/", parts[["path"]])

  # rebuild the url
  burl <- httr2::url_build(parts)

  # create the base request
  b_req <- httr2::req_url_path_append(
    httr2::request(burl),
    "truncate"
  )

  # create the request
  req <- httr2::req_body_form(
    b_req,
    f = "json",
    async = async,
    attachmentOnly = attachment_only
  )

  # perform request
  resp <- httr2::req_perform(
    # add the token
    httr2::req_auth_bearer_token(req, token)
    )

  res_string <- httr2::resp_body_string(resp)

  res <- RcppSimdJson::fparse(res_string)
  detect_errors(res)
  res
}
