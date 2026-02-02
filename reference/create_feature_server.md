# Create a FeatureServer

Creates an empty FeatureServer with no additional layers.

## Usage

``` r
create_feature_server(
  service_name,
  description = "",
  crs = 3857,
  capabilities = c("create", "delete", "query", "update", "editing"),
  query_formats = c("json", "geojson"),
  initial_extent = list(xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL),
  max_record_count = 1000L,
  allow_updates = TRUE,
  copyright = "",
  has_static_data = FALSE,
  xss_prevention = xss_defaults(),
  token = arc_token()
)

xss_defaults()
```

## Arguments

- service_name:

  Feature Service name.

- description:

  default blank. The description of the feature server.

- crs:

  default `3857`. A coordinate reference system to set for the feature
  server. Must be compatible with
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html).

- capabilities:

  default full capabilities. Character vector of capabilities.

- query_formats:

  default json and geojson. May be restricted by site-wide settings.

- initial_extent:

  optional. A named list with element of `xmin`, `xmax`, `ymin`, and
  `ymax`. Values must be in the same CRS as `crs`.

- max_record_count:

  default `1000`. The maximum number of records that can be retrieved
  from a layer in one request.

- allow_updates:

  default `TRUE`. Determine if geometries can be updated.

- copyright:

  default blank. Copyright notice to provide in the Feature Server

- has_static_data:

  default `FALSE`. Indicates if data is changing.

- xss_prevention:

  cross-site-scripting prevention is enabled by default. See details for
  more.

- token:

  an `httr2_token` as created by
  [`auth_code()`](https://rdrr.io/pkg/arcgisutils/man/auth.html) or
  similar

## Value

If a `FeatureServer` is created successfully, a `FeatureServer` object
is returned based on the newly created feature server's url.

## Details

**\[experimental\]**

## Examples

``` r
if (FALSE) { # \dontrun{
  set_arc_token(auth_code())
  create_feature_server("My empty feature server")
} # }
```
