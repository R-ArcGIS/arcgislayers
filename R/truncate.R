# set_auth_token(auth_code())
#
# g <- arc_open("https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/Guerry%20test%204/FeatureServer/0")

truncate_layer <- function(
    x,
    async = TRUE,
    attachment_only = FALSE,
    token = Sys.getenv("ARCGIS_TOKEN")
) {

  obj_check_layer(x)

  if (!x[["supportsTruncate"]]) {
    obj <- rlang::caller_arg(x)
    cli::cli_abort(
      "{.arg {obj}} does not support the {.code truncate } operation"
    )
  }

  b_req <- httr2::req_url_path_append(
    httr2::request(x[["url"]]),
    "truncate"
  )

  req <- httr2::req_body_form(
    b_req,
    async = async,
    attachmentOnly = attachment_only,
    token = token,
    f = "json"
  )

  resp <- httr2::req_perform(req)
  httr2::resp_body_string(resp)
}
