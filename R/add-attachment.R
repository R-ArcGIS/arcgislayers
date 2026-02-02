#' @param file_name the name of the file. Defaults to the `basename(path)`.
#' Must be the same length as `feature_id`.
#' @inheritParams arc_open
#' @references See [API documentation](https://developers.arcgis.com/rest/services-reference/enterprise/add-attachment/#request-parameters) for more.
#' @rdname attachments
#' @export
#' @examples
#' \dontrun{
#' if (interactive()) {
#' library(arcgisutils)
#'
#' # authenticate
#' set_arc_token(auth_user())
#'
#' # open a feature service
#' feature_layer <- arc_open("your-item-id") |>
#'   # layer ID of the feature service
#'   get_layer(0)
#'
#' # create a list of features to update
#' features <- c(1,2,3)
#'
#' # create a list of files to upload as attachments
#' attachment_files <- c("path/to/file1.png", "path/to/file2.png", "path/to/file3.png")
#'
#' # add the attachment files to the features in the feature layer
#' add_response <- add_attachments(feature_layer, features, attachment_files, use_basename=TRUE)
#' }
#' }

add_attachments <- function(
  x,
  feature_id,
  path,
  file_name = basename(path),
  .progress = TRUE,
  token = arc_token()
) {
  obj_check_layer(x)

  if (!rlang::is_character(feature_id) && !rlang::is_integer(feature_id)) {
    cli::cli_abort("{.arg feature_id} must be a character or integer vector")
  }

  if (anyNA(feature_id)) {
    cli::cli_abort("{.arg feature_id} must not contain missing values")
  }

  if (!rlang::is_character(path)) {
    cli::cli_abort("{.arg path} must be a character vector")
  }

  if (anyNA(path)) {
    cli::cli_abort("{.arg path} must not contain missing values")
  }

  if (!all(file.exists(path))) {
    cli::cli_abort("All files specified in {.arg path} must exist")
  }

  n <- length(path)

  if (n != length(feature_id)) {
    cli::cli_abort(
      "{.arg feature_id} and {.arg path} must be the same length"
    )
  }

  if (n != length(file_name)) {
    cli::cli_abort(
      "{.arg file_name} and {.arg path} must be the same length"
    )
  }

  url <- x[["url"]]

  if (is.null(url)) {
    cli::cli_abort("Feature Service URL was null. This is unexpected.")
  }

  all_reqs <- vector("list", n)

  for (i in seq_len(n)) {
    fid <- feature_id[i]
    p <- path[i]
    fname <- file_name[i]
    f <- curl::form_file(p, name = fname)

    all_reqs[[i]] <- arc_base_req(
      url,
      path = c(fid, "addAttachment"),
      token = token,
      query = c(f = "json")
    ) |>
      httr2::req_body_multipart(attachment = f)
  }

  all_resps <- httr2::req_perform_parallel(
    all_reqs,
    max_active = 3,
    progress = .progress,
    on_error = "continue"
  )

  all_resps_body <- lapply(
    httr2::resps_successes(all_resps),
    function(.x) {
      r <- httr2::resp_body_string(.x)
      cnd <- catch_error(r)

      if (rlang::is_condition(cnd)) {
        cnd$call <- rlang::caller_call(2)
        print(cnd)
        return(NULL)
      }
      as.data.frame(compact(RcppSimdJson::fparse(r)[[1]]))
    }
  )

  res <- rbind_results(all_resps_body)

  errors <- httr2::resps_failures(all_resps)
  n_errs <- length(errors)
  if (n_errs > 0) {
    cli::cli_warn(
      "{n_errs} occured. Error responses are stored in the `errors` attribute"
    )
    attr(res, "errors") <- errors
  }

  res
}
