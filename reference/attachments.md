# Query and Download Feature Service Attachments

Query attachment information using `query_layer_attachments()` and
download attachments using `download_attachments()`.

## Usage

``` r
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
```

## Arguments

- x:

  an object of class `FeatureLayer`, `Table`, or `ImageServer`.

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

- token:

  an `httr2_token` as created by `auth_code()` or similar

- attachments:

  a `data.frame` created by `query_layer_attachments()`. Must contain
  the columns `name`, `url`, and `contentType`.

- out_dir:

  the path to the folder to download the file

- overwrite:

  default `FALSE`. A

- .progress:

  default `TRUE.` Whether a progress bar should be provided.

## Value

`query_layer_attachments()` returns a data.frame.

`download_attachments()` returns a list. If an error occurs, the
condition is captured and returned in the list. Otherwise the path to
the file that was downloaded is returned.

## References

[ArcGIS REST API
Documentation](https://developers.arcgis.com/rest/services-reference/enterprise/query-attachments-feature-service-layer/)

## Examples

``` r
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
```
