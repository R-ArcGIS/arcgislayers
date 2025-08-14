# https://developers.arcgis.com/rest/services-reference/enterprise/update-attachment/#request-parameters

update_attachments <- function(
  x,
  attachment_id,
  path,
  .progress = TRUE,
  token = arc_token()
) {
  # ensure it is a feature service
  obj_check_layer(x)

  if (
    !rlang::is_character(attachment_id) && !rlang::is_integer(attachment_id)
  ) {
    cli::cli_abort("{.arg attachment_id} must be a character or integer vector")
  }

  if (anyNA(attachment_id)) {
    cli::cli_abort("{.arg attachment_id} must not contain missing values")
  }

  if (!rlang::is_character(path)) {
    cli::cli_abort("{.arg path} must be a character vector")
  }

  if (anyNA(path)) {
    cli::cli_abort("{.arg path} must not contain missing values")
  }

  if (!all(file.exists(path))) {
    cli::cli_abort("All files specified in {arg path} must exist")
  }

  n <- length(attachment_id)

  if (n != length(path)) {
    cli::cli_abort(
      "{.arg attachment_id} and {.arg path} must be the same length"
    )
  }

  all_reqs <- vector("list", n)

  url <- x[["url"]]

  if (is.null(url)) {
    cli::cli_abort("Feature Service URL was null. This is unexpected.")
  }

  for (i in seq_len(n)) {
    f <- curl::form_file(path[i], name = basename(path[i]))
    aid <- attachment_id[i]
    req <- arc_base_req(
      url,
      path = "updateAttachments",
      token = arc_token()
    ) |>
      httr2::req_body_multipart(
        attachmentId = as.character(aid),
        attachment = f,
        f = "json"
      )

    all_reqs[[i]] <- req
  }

  all_resps <- httr2::req_perform_parallel(
    all_reqs,
    max_active = 3,
    progress = .progress,
    on_error = "continue"
  )

  all_resps_body <- lapply(all_resps, \(.x) {
    r <- httr2::resp_body_string(.x)
    cnd <- catch_error(r)

    if (rlang::is_condition(cnd)) {
      cnd$call <- rlang::caller_call(2)
      print(cnd)
    }
    RcppSimdJson::fparse(r)
  })

  errors <- httr2::resps_failures(all_resps)
  n_errs <- length(errors)
  if (n_errs > 0) {
    cli::cli_warn("{n_errs} occured. Returning error responses")
    return(errors)
  }
}
