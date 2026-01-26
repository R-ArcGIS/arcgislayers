# Update Feature Service Attachments

Feature Services can contain attachments that are associated with a
single feature ID.
[`update_features()`](https://developers.arcgis.com/r-bridge/reference/modify.md)
enables you to update the attachments of multiple features at once by
generating multiple update requests and performing them in parallel.

## Usage

``` r
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

- attachment_id:

  the ID of the attachment—this corresponds to the `id` column returned
  from
  [`query_layer_attachments()`](https://developers.arcgis.com/r-bridge/reference/attachments.md)

- path:

  a vecetor of the same length as `feature_id` indicating where the
  attachment exists.

- .progress:

  default `TRUE.` Whether a progress bar should be provided.

- token:

  an `httr2_token` as created by
  [`auth_code()`](https://rdrr.io/pkg/arcgisutils/man/auth.html) or
  similar

## Value

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
