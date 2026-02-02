# Query and Download Feature Service Attachments

Query attachment information using `query_layer_attachments()` and
download attachments using `download_attachments()`.

Feature Services can contain attachments that are associated with a
single feature ID.

- Use
  [`add_features()`](https://developers.arcgis.com/r-bridge/reference/modify.md)
  to add attachments to a feature service

- Use
  [`update_features()`](https://developers.arcgis.com/r-bridge/reference/modify.md)
  to update the attachments of a feature service

- Use `query_layer_attachments()` to list attachments of a feature
  service

- Use `download_attachments()` with the results of
  `query_layer_attachments()` to download the attachments from a feature
  service locally

## Usage

``` r
add_attachments(
  x,
  feature_id,
  path,
  file_name = basename(path),
  .progress = TRUE,
  token = arc_token()
)

query_layer_attachments(
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
)

download_attachments(
  attachments,
  out_dir,
  ...,
  overwrite = FALSE,
  .progress = TRUE,
  token = arc_token()
)

update_attachments(
  x,
  feature_id,
  attachment_id,
  path,
  .progress = TRUE,
  token = arc_token()
)
```

## Arguments

- x:

  an object of class `FeatureLayer`, `Table`, or `ImageServer`.

- feature_id:

  a vector of object IDs that corresponds to the feature of the
  corresponding `attachment_id`.

- path:

  a vecetor of the same length as `feature_id` indicating where the
  attachment exists.

- file_name:

  the name of the file. Defaults to the `basename(path)`. Must be the
  same length as `feature_id`.

- .progress:

  default `TRUE.` Whether a progress bar should be provided.

- token:

  an `httr2_token` as created by
  [`auth_code()`](https://rdrr.io/pkg/arcgisutils/man/auth.html) or
  similar

- definition_expression:

  default `1 = 1`. A SQL where clause that is applied to the layer. Only
  those records that conform to this expression will be returned. This
  parameter is required if neither `object_ids` or `global_ids` have
  been defined.

- attachments_definition_expression:

  default `NULL`. A SQL where calsue that is applied to the attachment
  metadata. only attachments that conform to this expression will be
  returned.

- object_ids:

  mutually exclusive with `definition_expression` and `global_ids`. The
  object IDs of the features to query attachments of.

- global_ids:

  mutally exclusive with `definition_expression` and `object_ids`. The
  global IDs of the features to query attachments of.

- attachment_types:

  default `NULL`. A character vector of attachment types to filter on.

- keywords:

  default `NULL`. A character vector of the keywords to filter on.

- return_metadata:

  default `TRUE`. Returns metadata stored in the `exifInfo` field.

- ...:

  unused

- attachments:

  a `data.frame` created by `query_layer_attachments()`. Must contain
  the columns `name`, `url`, and `contentType`.

- out_dir:

  the path to the folder to download the file

- overwrite:

  default `FALSE`. A

- attachment_id:

  the ID of the attachment—this corresponds to the `id` column returned
  from `query_layer_attachments()`

## Value

`query_layer_attachments()` returns a data.frame.

`download_attachments()` returns a list. If an error occurs, the
condition is captured and returned in the list. Otherwise the path to
the file that was downloaded is returned.

a `data.frame` with 2 columns returning the status of the update.

## Details

**\[experimental\]** To rename or otherwise modify an attachment in a
Feature Service, you must first download that attachment, modify the
file on disk, and then upload it again. This is a limitation of ArcGIS
Online and Enterprise. If you'd like to see this changed, please submit
a community idea at
[community.esri.com](https://community.esri.com/t5/arcgis-online/ct-p/arcgis-online).

If any requests fail, the requests are added as as the `errors`
attribute to the resultant `data.frame`.

## References

See [API
documentation](https://developers.arcgis.com/rest/services-reference/enterprise/add-attachment/#request-parameters)
for more.

[ArcGIS REST API
Documentation](https://developers.arcgis.com/rest/services-reference/enterprise/query-attachments-feature-service-layer/)

See [API
documentation](https://developers.arcgis.com/rest/services-reference/enterprise/update-attachment/#request-parameters)
for more.

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive()) {
library(arcgisutils)

# authenticate
set_arc_token(auth_user())

# open a feature service
feature_layer <- arc_open("your-item-id") |>
  # layer ID of the feature service
  get_layer(0)

# create a list of features to update
features <- c(1,2,3)

# create a list of files to upload as attachments
attachment_files <- c("path/to/file1.png", "path/to/file2.png", "path/to/file3.png")

# add the attachment files to the features in the feature layer
add_response <- add_attachments(feature_layer, features, attachment_files, use_basename=TRUE)
}
} # }
if (FALSE) { # \dontrun{
# create a url path that isn't too wide for CRAN
furl <- paste(
  c(
    "https://services1.arcgis.com/hLJbHVT9ZrDIzK0I",
    "arcgis/rest/services/v8_Wide_Area_Search_Form_Feature_Layer___a2fe9c",
    "FeatureServer/0"
  ),
  collapse = "/"
)
# connect to the layer
layer <- arc_open(furl)

# get the attachment info
att <- query_layer_attachments(layer)

# download them to a path
download_attachments(att, "layer_attachments")
} # }
if (FALSE) { # \dontrun{
if (interactive()) {
library(arcgisutils)

# authenticate
set_arc_token(auth_user())

# open a feature service
feature_layer <- arc_open("your-item-id") |>
  # layer ID of the feature service
  get_layer(0)

# query attachment layer information
attachments <- query_layer_attachments(feature_layer)

# create a temporary directory
tmp <- tempdir()

# download attachments to the temporary directory
download_attachments(attachments, tmp)

# get original paths
fps <- file.path(tmp, attachments$name)

# prepend attachments with the date
new_filenames <- paste0(Sys.Date(), "-", basename(attachments$name))

# create new file paths
new_fps <- file.path(dirname(fps), new_filenames)

# rename the files
file.rename(fps, new_fps)

# update the attachments
update_res <- update_attachments(
  feature_layer,
  # OID of the feature <> attachment relationship
  attachments$parentObjectId,
  # the attachment ID
  attachments$id,
  # the path to the attachment on disk
  new_fps
)
}
} # }
```
