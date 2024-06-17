library(arcgis)

set_arc_token(auth_user())

flayer <- arc_open("https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/survey123_b32978c284d74e1c9dd4a6f5fde28155/FeatureServer/0")

flayer


# Attachment types
attachment_types <- c(
  "7z", "aif", "avi", "bmp", "csv", "doc", "docx", "dot", "ecw", "emf", "eps",
  "geodatabase", "geojson", "gif", "gml", "gtar", "gz", "img", "j2k", "jp2",
  "jpc", "jpe", "jpeg", "jpf", "jpg", "json", "m4a", "mdb", "mid", "mov", "mp2",
  "mp3", "mp4", "mpa", "mpe", "mpeg", "mpg", "mpv2", "pdf", "png", "ppt",
  "pptx", "ps", "psd", "qt", "ra", "ram", "raw", "rmi", "sid", "tar", "tgz",
  "tif", "tiff", "txt", "vrml", "wav", "wma", "wmf", "wmv", "wps", "xls",
  "xlsx", "xlt", "xml", "zip"
)

attachment_types

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
