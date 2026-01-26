# Read from an Image Server

Given an `ImageServer` export an image as a terra `SpatRaster` object.
See
[`terra::rast`](https://rspatial.github.io/terra/reference/rast.html).

## Usage

``` r
arc_raster(
  x,
  xmin,
  xmax,
  ymin,
  ymax,
  bbox_crs = NULL,
  crs = sf::st_crs(x),
  width = NULL,
  height = NULL,
  format = "tiff",
  ...,
  raster_fn = NULL,
  token = arc_token()
)
```

## Arguments

- x:

  an `ImageServer` as created with
  [`arc_open()`](https://developers.arcgis.com/r-bridge/reference/arc_open.md).

- xmin:

  the minimum bounding longitude value.

- xmax:

  the maximum bounding longitude value.

- ymin:

  that minimum bounding latitude value.

- ymax:

  the maximum bounding latitude value.

- bbox_crs:

  the CRS of the values passed to `xmin`, `xmax`, `ymin`, and `ymax`. If
  not specified, uses the CRS of `x`.

- crs:

  the CRS of the resultant raster image and the provided bounding box
  defined by `xmin`, `xmax`, `ymin`, `ymax` (passed `outSR` query
  parameter).

- width:

  default `NULL`. Cannot exceed `x[["maxImageWidth"]]`.

- height:

  default `NULL`. Cannot exceed `x[["maxImageHeight"]]`.

- format:

  default `"tiff"`. Must be one of "jpgpng", "png", "png8", "png24",
  "jpg", "bmp", "gif", "tiff", "png32", "bip", "bsq", "lerc".

- ...:

  additional key value pairs to be passed to
  [`httr2::req_body_form()`](https://httr2.r-lib.org/reference/req_body.html).

- raster_fn:

  a scalar string with the name of the service's raster function. See
  [`list_raster_fns()`](https://developers.arcgis.com/r-bridge/reference/raster_fns.md)
  for available raster functions.

- token:

  default `arc_token()` authorization token.

## Value

An object of class `SpatRaster`.

## Details

**\[experimental\]**

## Examples

``` r
if (FALSE) { # \dontrun{
img_url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"

landsat <- arc_open(img_url)

arc_raster(
  landsat,
  xmin = -71,
  xmax = -67,
  ymin = 43,
  ymax = 47.5,
  bbox_crs = 4326,
  width = 100,
  height = 100
)
} # }
```
