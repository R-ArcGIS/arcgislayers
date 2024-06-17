library(arcgis)

set_arc_token(auth_user())

flayer <- arc_open("https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/survey123_b32978c284d74e1c9dd4a6f5fde28155/FeatureServer/0")

flayer


query_layer_attachments <- function(
    layer,
    definition_expression = "1=1",
    object_ids = NULL,
    global_ids = NULL,
    attachment_types = NULL,
    keywords = NULL,
    ...,
    token = arc_token()
    # Ignored arguments for now:
    # returnMetadata, size,
    ) {
  b_req <- arc_base_req(layer[["url"]], token = token, path = "queryAttachments")

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

  httr2::req_body_form(
    b_req,
    objectIds = object_ids,
    globalIds = global_ids,
    attachmentTypes = attachment_types,
    definitionExpression = definition_expression,
    keywords = keywords,
    returnUrl = TRUE,
    f = "json"
  )
}



# Keywords must be comma separated if there are multiple

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
possible_attachment_types

# definitionExpression
# This matches the where clause of a query
# definitionExpression=STATE_NAME = 'Alaska'

attachment_info <- arc_base_req(flayer$url, token = arc_token(), path = "queryAttachments") |>
  httr2::req_body_form(
    # use 1=1 to find all of the attachments
    definitionExpression = "1=1",
    returnUrl = TRUE,
    f = "json"
  ) |>
  httr2::req_perform() |>
  httr2::resp_body_string() |>
  RcppSimdJson::fparse()

attachment_info$fields

res <- attachment_info$attachmentGroups |>
  janitor::clean_names() |>
  structure(class = c("tbl", "data.frame"))

as_tbl <- function(x) structure(x, class = c("tbl", "data.frame"))

tidyr::unnest(res, attachment_infos) |>
  as_tbl()


# The idea will be that you can query attachments from a flayer
# This wil work on a **layer**
# query_layer_attachments(layer, ...)
# then to download the attachments you can then use `download_attachments()`
# which will require the URL to the attachment

# This will work on a **Feature** of a **layer**
# query_feature_attachments(layer, feature_id)


# flayer <- arc_open("https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/survey123_b32978c284d74e1c9dd4a6f5fde28155/FeatureServer/0")

# attachment_info <- flayer |>
#   query_layer_attachments(tidy = TRUE)

# download_attachments(
#   attachment_info$url_column,
#   outdir = "./downloads"
# )
