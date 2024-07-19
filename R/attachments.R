#' Query and download attachments
#'
#' Get metadata about attachments associated with features in a layer.
#' Query attachment information using `query_layer_attachments()` and
#' download attachments using `download_attachments()`.
#'
#' @inheritParams arc_select
#' @param definition_expression default `1 = 1`. A SQL where clause that is applied to the layer. Only those records that conform to this expression will be returned. This parameter is required if neither `object_ids` or `global_ids` have been defined.
#' @param attachments_definition_expression default `NULL`. A SQL where calsue that is applied to the attachment metadata.
#'   only attachments that conform to this expression will be returned.
#' @param object_ids mutually exclusive with `definition_expression` and `global_ids`. The object IDs of the features to query attachments of.
#' @param global_ids mutally exclusive with `definition_expression` and `object_ids`. The global IDs of the features to query attachments of.
#' @param keywords default `NULL`. A character vector of the keywords to filter on.
#' @param attachment_types default `NULL`. A character vector of attachment types to filter on.
#' @param return_metadata default `TRUE`. Returns metadata stored in the `exifInfo` field.
#' @param overwrite default `FALSE`. A
#' @rdname attachments
#' @references [ArcGIS REST API Documentation](https://developers.arcgis.com/rest/services-reference/enterprise/query-attachments-feature-service-layer/)
#' @export
#' @returns
#' `query_layer_attachments()` returns a data.frame.
#'
#' `download_attachments()` returns a list. If an error occurs, the condition is captured and returned in the list.
#' Otherwise the path to the file that was downloaded is returned.
#' @examples
#' \dontrun{
#' # create a url path that isn't too wide for CRAN
#' furl <- paste(
#'   c(
#'     "https://services1.arcgis.com/hLJbHVT9ZrDIzK0I",
#'     "arcgis/rest/services/v8_Wide_Area_Search_Form_Feature_Layer___a2fe9c",
#'     "FeatureServer/0"
#'   ),
#'   collapse = "/"
#' )
#' # connect to the layer
#' layer <- arc_open(furl)
#'
#' # get the attachment info
#' att <- query_layer_attachments(layer)
#'
#' # download them to a path
#' download_attachments(att, "layer_attachments")
#' }
query_layer_attachments <- function(
    x,
    definition_expression = "1=1",
    attachments_definition_expression = NULL,
    object_ids = NULL,
    global_ids = NULL,
    attachment_types = NULL,
    keywords = NULL,
    return_metadata = TRUE,
    ...,
    token = arc_token()
    # Ignored arguments for now:
    # returnMetadata, size,
    ) {
  check_string(definition_expression, allow_null = TRUE)
  check_string(attachments_definition_expression, allow_null = TRUE)
  check_character(global_ids, allow_null = TRUE)
  # TODO validate that object_ids is a vector

  # ensure that attachments are available.
  if (!x[["hasAttachments"]]) {
    cli::cli_abort("{.arg layer} does not support attachments.")
  }

  # check that only one of definition_expression, object_ids, and global_ids is provided
  rlang::check_exclusive(definition_expression, object_ids, global_ids, .require = FALSE)

  # validate attachment types if provided
  if (!is.null(attachment_types)) {
    rlang::arg_match(attachment_types, possible_attachment_types, multiple = TRUE)
  }

  # check keywords
  check_character(keywords, allow_empty = FALSE, allow_null = TRUE)

  # if keywords is greater than length 1 then we paste it together
  if (length(keywords) > 1) {
    keywords <- paste(keywords, collapse = ",")
  }

  # create the base request
  b_req <- arc_base_req(x[["url"]], token = token, path = "queryAttachments")

  # Fill the body of the request
  req <- httr2::req_body_form(
    b_req,
    objectIds = paste(object_ids, collapse = ","),
    globalIds = paste(global_ids, collapse = ","),
    attachmentTypes = attachment_types,
    definitionExpression = definition_expression,
    attachmentsDefinitionExpression = attachments_definition_expression,
    keywords = keywords,
    returnUrl = TRUE,
    returnMetadata = return_metadata,
    f = "json"
  )

  # send request
  resp <- httr2::req_perform(req)
  # parse response
  res <- RcppSimdJson::fparse(httr2::resp_body_string(resp))
  # throw an error if they are there
  arcgisutils::detect_errors(res)
  # TODO consider importing {heck} for name cleaning later
  unnest_attachment_groups(res$attachmentGroups)
}

#' Helper to unnest attachment info results
#' @param x the attachmentGroups column from the query results
#' @noRd
unnest_attachment_groups <- function(x) {
  if (is.null(x[["attachmentInfos"]])) {
    cli::cli_alert_info("No attachments found.")
    return(NULL)
  }

  n_elem <- vapply(x[["attachmentInfos"]], nrow, integer(1))
  res <- cbind(
    x[rep.int(1:nrow(x), n_elem), c("parentGlobalId", "parentObjectId")],
    do.call(rbind, x[["attachmentInfos"]])
  )
  data_frame(res)
}


# Attachment types
possible_attachment_types <- c(
  "7z", "aif", "avi", "bmp", "csv", "doc", "docx", "dot", "ecw", "emf", "eps",
  "geodatabase", "geojson", "gif", "gml", "gtar", "gz", "img", "j2k", "jp2",
  "jpc", "jpe", "jpeg", "jpf", "jpg", "json", "m4a", "mdb", "mid", "mov", "mp2",
  "mp3", "mp4", "mpa", "mpe", "mpeg", "mpg", "mpv2", "pdf", "png", "ppt",
  "pptx", "ps", "psd", "qt", "ra", "ram", "raw", "rmi", "sid", "tar", "tgz",
  "tif", "tiff", "txt", "vrml", "wav", "wma", "wmf", "wmv", "wps", "xls",
  "xlsx", "xlt", "xml", "zip"
)


#' @export
#' @rdname attachments
#' @param attachments a `data.frame` created by `query_layer_attachments()`. Must contain the columns `name`, `url`, and `contentType`.
#' @param ... unused
#' @param .progress default `TRUE.` Whether a progress bar should be provided.
#' @param out_dir the path to the folder to download the file
download_attachments <- function(
    attachments,
    out_dir,
    ...,
    overwrite = FALSE,
    .progress = TRUE,
    token = arc_token()) {
  # check that the input is a data frame with the appropriate types
  # how can we generalize this a bit more?
  # check_df_cols(
  #    df, col ~ check_character, col ~ check_numeric
  # )
  check_data_frame(attachments)
  check_bool(overwrite)
  check_bool(.progress)

  if (is.null(attachments[["name"]])) {
    cli::cli_abort(
      c(
        "{.val name} is missing from {.arg attachments} argument.",
        "i" = "provide the output from {.fn query_layer_attachments}"
      )
    )
  } else {
    check_character(
      attachments[["name"]],
      allow_empty = FALSE,
      allow_na = FALSE,
      allow_null = FALSE
    )
  }

  if (is.null(attachments[["url"]])) {
    cli::cli_abort(
      c(
        "{.val url} is missing from {.arg attachments} argument.",
        "i" = "provide the output from {.fn query_layer_attachments}"
      )
    )
  } else {
    check_character(
      attachments[["url"]],
      allow_empty = FALSE,
      allow_na = FALSE,
      allow_null = FALSE
    )
  }

  # ensure that we still have the content-type this is going to be used
  # to check that the the resopns type is the correct mime type
  if (is.null(attachments[["contentType"]])) {
    cli::cli_abort(
      c(
        "{.val contentType} is missing from {.arg attachments} argument.",
        "i" = "provide the output from {.fn query_layer_attachments}"
      )
    )
  } else {
    check_character(
      attachments[["contentType"]],
      allow_empty = FALSE,
      allow_na = FALSE,
      allow_null = FALSE
    )
  }

  # Create the output directory if it doesn't yet exists
  if (!dir.exists(out_dir)) {
    cli::cli_inform("Directory {.path {out_dir}} does not exist. Creating folders.")
    dir.create(out_dir, recursive = TRUE)
  }

  # create the output path names
  out_fps <- file.path(out_dir, attachments[["name"]])
  already_exist <- file.exists(out_fps)

  # required fields
  urls <- attachments[["url"]]
  content_types <- attachments[["contentType"]]

  if (!overwrite && any(already_exist)) {
    # Files with the same name found.
    cli::cli_inform(
      c(
        "Files with the same name found in {.path {out_dir}}",
        "i" = "Existing files: {.file {out_fps[already_exist]}}",
        ">" = "set {.arg overwrite = TRUE} to overwrite these files"
      )
    )
    # subset to the files that dont already exist
    out_fps <- out_fps[!already_exist]
    urls <- urls[!already_exist]
    content_types <- content_types[!already_exist]
  }
  # TODO check if files exist, if they do provide a message
  # saying we're skipping downloading.
  # To download use `overwrite = TRUE`

  # create the requests
  attachment_reqs <- lapply(urls, arc_base_req, token = token)

  # perform the requests
  resps <- httr2::req_perform_parallel(
    attachment_reqs,
    on_error = "continue",
    progress = .progress
  )


  Map(.download_attachment, resps, content_types, out_fps)
}

.download_attachment <- function(.resp, .content_type, .fp) {
  httr2::resp_check_content_type(.resp, .content_type)
  # if the content types don't match, we'll catch the error here
  # otherwise return a NULL
  cnd <- rlang::catch_cnd(
    httr2::resp_check_content_type(.resp, .content_type)
  )

  # return early
  if (!is.null(cnd)) {
    return(cnd)
  }

  # otherwise, we continue and extract the bytes and write to
  # the file
  writeBin(httr2::resp_body_raw(.resp), .fp)
  invisible(.fp)
}
