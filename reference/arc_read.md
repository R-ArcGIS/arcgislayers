# Read an ArcGIS FeatureLayer, Table, or ImageServer

`arc_read()` combines the functionality of
[`arc_open()`](https://developers.arcgis.com/r-bridge/reference/arc_open.md)
with
[`arc_select()`](https://developers.arcgis.com/r-bridge/reference/arc_select.md)
or
[`arc_raster()`](https://developers.arcgis.com/r-bridge/reference/arc_raster.md)
to read an ArcGIS `FeatureLayer`, `Table`, or `ImageServer` to an `sf`
or `SpatRaster` object. Optionally, set, check, or modify names for the
returned data frame or sf object using the `col_names` and `name_repair`
parameters. For ease of use and convenience, `arc_read()` allows users
to access and query a FeatureLayer, Table, or ImageServer with a single
function call instead of combining
[`arc_open()`](https://developers.arcgis.com/r-bridge/reference/arc_open.md)
and
[`arc_select()`](https://developers.arcgis.com/r-bridge/reference/arc_select.md).
The conventions of `col_select` are based on functions for reading
tabular data in the `{readr}` package.

## Usage

``` r
arc_read(
  url,
  col_names = TRUE,
  col_select = NULL,
  n_max = Inf,
  name_repair = "unique",
  crs = NULL,
  ...,
  fields = NULL,
  alias = "drop",
  token = arc_token()
)
```

## Arguments

- url:

  a url to a service such as a feature service, image server, or map
  server. Alternatively, an item ID of a portal item or portal url.

- col_names:

  Default `TRUE`. Column names or name handling rule. `col_names` can be
  `TRUE`, `FALSE`, `NULL`, or a character vector:

  - If `TRUE`, use existing default column names for the layer or table.
    If `FALSE` or `NULL`, column names will be generated automatically:
    X1, X2, X3 etc.

  - If `col_names` is a character vector, values replace the existing
    column names. `col_names` can't be length 0 or longer than the
    number of fields in the returned layer.

- col_select:

  Default `NULL`. A character vector of the field names to be returned.
  By default, all fields are returned.

- n_max:

  Defaults to `Inf` or an option set with
  `options("arcgislayers.n_max" = <max records>)`. Maximum number of
  records to return.

- name_repair:

  Default `"unique"`. See
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for details. If `name_repair = NULL` and `alias = "replace"` may
  include invalid names.

- crs:

  the spatial reference to be returned. If the CRS is different than the
  CRS for the input `FeatureLayer`, a transformation will occur
  server-side. Ignored if x is a `Table`.

- ...:

  Additional arguments passed to
  [`arc_select()`](https://developers.arcgis.com/r-bridge/reference/arc_select.md)
  if URL is a `FeatureLayer` or `Table` or
  [`arc_raster()`](https://developers.arcgis.com/r-bridge/reference/arc_raster.md)
  if URL is an `ImageLayer`.

- fields:

  Default `NULL`. a character vector of the field names to returned. By
  default all fields are returned. Ignored if `col_names` is supplied.

- alias:

  Use of field alias values. Default `c("drop", "label", "replace"),`.
  There are three options:

  - `"drop"`, field alias values are ignored.

  - `"label"`: field alias values are assigned as a label attribute for
    each field.

  - `"replace"`: field alias values replace existing column names.
    `col_names`

- token:

  an `httr2_token` as created by `auth_code()` or similar

## Value

An sf object, a `data.frame`, or an object of class `SpatRaster`.

## Details

**\[experimental\]**

## See also

[`arc_select()`](https://developers.arcgis.com/r-bridge/reference/arc_select.md);
[`arc_raster()`](https://developers.arcgis.com/r-bridge/reference/arc_raster.md)

## Examples

``` r
if (FALSE) { # \dontrun{
furl <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3"

# read entire service
arc_read(furl)

# apply tolower() to column names
arc_read(url, name_repair = tolower)

# use paste0 to prevent CRAN check NOTE
furl <- paste0(
  "https://sampleserver6.arcgisonline.com/arcgis/rest/services/",
  "EmergencyFacilities/FeatureServer/0"
)

# use field aliases as column names
arc_read(furl, alias = "replace")

# read an ImageServer directly
img_url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"

arc_read(
  img_url,
  width = 100, height = 100,
  xmin = -71, ymin = 43,
  xmax = -67, ymax = 47.5,
  bbox_crs = 4326
)
} # }
```
