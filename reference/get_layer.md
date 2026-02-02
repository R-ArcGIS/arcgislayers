# Extract a layer from a Feature or Map Server

These helpers provide easy access to the layers contained in a
`FeatureServer`, `MapServer`, or `GroupLayer`.

## Usage

``` r
get_layer(x, id = NULL, name = NULL, token = arc_token())

get_all_layers(x, token = arc_token())

get_layers(x, id = NULL, name = NULL, token = arc_token())
```

## Arguments

- x:

  an object of class `FeatureServer`, `MapServer`, or `GroupLayer`.

- id:

  default `NULL`. A numeric vector of unique ID of the layer you want to
  retrieve. This is a scalar in `get_layer()`.

- name:

  default `NULL`. The name associated with the layer you want to
  retrieve. `name` is mutually exclusive with `id`. This is a scalar in
  `get_layer()`.

- token:

  an `httr2_token` as created by
  [`auth_code()`](https://rdrr.io/pkg/arcgisutils/man/auth.html) or
  similar

## Value

- `get_layer()` returns a single `FeatureLayer` or `Table` based on its
  ID

- `get_layers()` returns a list of the items specified by the `id` or
  `name` argument

- `get_all_layers()` returns a named `list` with an element `layers` and
  `tables`. Each a list containing `FeatureLayer` and `Table`s
  respectively.

## Details

**\[experimental\]**

The `id` and `name` arguments must match the field values of the
respective names as seen in the output of
[`list_items()`](https://developers.arcgis.com/r-bridge/reference/utils.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # FeatureServer
  furl <- paste0(
    "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/",
    "PLACES_LocalData_for_BetterHealth/FeatureServer"
  )

  fserv <- arc_open(furl)

  fserv
  get_layer(fserv, 0)
  get_layers(fserv, name = c("Tracts", "ZCTAs"))
  get_all_layers(fserv)
} # }
```
