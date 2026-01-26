# Encode Domain Values

`encode_field_values()` can replace column values based on `codedValue`
type field domains from a corresponding `Table` or `FeatureLayer` object
created with
[`arc_open()`](https://developers.arcgis.com/r-bridge/reference/arc_open.md).

## Usage

``` r
encode_field_values(
  .data,
  .layer,
  field = NULL,
  codes = c("replace", "replace-valid", "label"),
  call = rlang::caller_env()
)
```

## Arguments

- .data:

  A data frame returned by
  [`arc_select()`](https://developers.arcgis.com/r-bridge/reference/arc_select.md)
  or
  [`arc_read()`](https://developers.arcgis.com/r-bridge/reference/arc_read.md).

- .layer:

  A Table or FeatureLayer object. Required.

- field:

  Optional character vector with names of fields to replace. Fields that
  do not have coded value domains are ignored. Defaults to `NULL` to
  replace or label all fields with coded value domains.

- codes:

  Use of field alias values. Defaults to `"replace"`. There are three
  options:

  - `"replace"`: coded values replace existing column values. Users are
    warned if the selected fields contain any non-coded values and these
    values are replaced with `NA`.

  - `"replace-valid"`: coded values replace existing *valid* column
    values. Any non-coded values remaing in place and are coerced to
    character.

  - `"label"`: coded values are applied as value labels via a `"label"`
    attribute.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

A data.frame with fields encoded with their respective domains.

## Examples

``` r
# \donttest{
layer <- arc_open(
  "https://geodata.baltimorecity.gov/egis/rest/services/Housing/dmxOwnership/MapServer/0"
)

res <- arc_select(
  layer,
  n_max = 100,
  where = "RESPAGCY <> '  '",
  fields = "RESPAGCY"
)
#> ℹ Query results limited to 100 out of 10397 available features.
encoded <- encode_field_values(res, layer)
table(encoded$RESPAGCY)
#> 
#>                 Education                   NPA/HCD Office of the Comptroller 
#>                         4                        83                        13 
# }
```
