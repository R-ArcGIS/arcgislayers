#' Update Feature Service Attachments
#'
#' Feature Services can contain attachments that are associated with a single feature ID.
#' `update_features()` enables you to update the attachments of multiple features at once
#' by generating multiple update requests and performing them in parallel.
#'
#' @details
#'  `r lifecycle::badge("experimental")`
#' To rename or otherwise modify an attachment in a Feature Service, you must first download
#' that attachment, modify the file on disk, and then upload it again. This is a limitation
#' of ArcGIS Online and Enterprise. If you'd like to see this changed, please submit a community idea at [community.esri.com](https://community.esri.com/t5/arcgis-online/ct-p/arcgis-online).
#'
#' If any requests fail, the requests are added as as the `errors` attribute to the resultant `data.frame`.
#' @param path a vecetor of the same length as `feature_id` indicating where the attachment exists.
#' @inheritParams arc_open
#' @inheritParams download_attachments
#' @param feature_id a vector of object IDs that corresponds to the feature of the corresponding `attachment_id`.
#' @param attachment_id the ID of the attachment—this corresponds to the `id` column returned from `query_layer_attachments()`
#' @returns a `data.frame` with 2 columns returning the status of the update.
#' @references See [API documentation](https://developers.arcgis.com/rest/services-reference/enterprise/update-attachment/#request-parameters) for more.
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
#' # query attachment layer information
#' attachments <- query_layer_attachments(feature_layer)
#'
#' # create a temporary directory
#' tmp <- tempdir()
#'
#' # download attachments to the temporary directory
#' download_attachments(attachments, tmp)
#'
#' # get original paths
#' fps <- file.path(tmp, attachments$name)
#'
#' # prepend attachments with the date
#' new_filenames <- paste0(Sys.Date(), "-", basename(attachments$name))
#'
#' # create new file paths
#' new_fps <- file.path(dirname(fps), new_filenames)
#'
#' # rename the files
#' file.rename(fps, new_fps)
#'
#' # update the attachments
#' update_res <- update_attachments(
#'   feature_layer,
#'   # OID of the feature <> attachment relationship
#'   attachments$parentObjectId,
#'   # the attachment ID
#'   attachments$id,
#'   # the path to the attachment on disk
#'   new_fps
#' )
#' }
#' }
update_attachments <- function(
  x,
  feature_id,
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

  if (!rlang::is_character(feature_id) && !rlang::is_integer(feature_id)) {
    cli::cli_abort("{.arg feature_id} must be a character or integer vector")
  }

  if (anyNA(feature_id)) {
    cli::cli_abort("{.arg feature_id} must not contain missing values")
  }

  # broadcast in the case of a scalar
  feature_id <- broadcast(feature_id, attachment_id)

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
    fid <- feature_id[i]
    req <- arc_base_req(
      url,
      path = c(fid, "updateAttachment"),
      token = token,
      query = c(f = "json")
    ) |>
      httr2::req_body_multipart(
        attachmentId = as.character(aid),
        attachment = f,
      )
    all_reqs[[i]] <- req
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


#' Broadcast x to the same length as y
#'
#' Broadcasts the argument `x` to the same length as `y`.
#'
#' @param x a scalar atomic or an atomic of the same length as `y`
#' @param y an atomic vector
#' @noRd
#' @keywords internal
broadcast <- function(x, y) {
  if (!rlang::is_bare_atomic(x) || !rlang::is_bare_atomic(y)) {
    rlang::abort("`x` and `y` must be atomic vectors")
  }

  if (typeof(x) != typeof(y)) {
    rlang::abort("`x` and `y` must be the same type")
  }

  len_y <- length(y)
  len_x <- length(x)

  if (len_x == 1L) {
    return(rep(x, len_y))
  }

  if (len_x != len_y) {
    rlang::abort("`x` must be a scalar or the same length as `y`")
  }

  x
}
