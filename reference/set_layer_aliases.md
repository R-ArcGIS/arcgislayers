# Set column labels or names based FeatureLayer or Table data frame field aliases

`set_layer_aliases()` can replace or label column names based on the the
field aliases from a corresponding `Table` or `FeatureLayer` object
created with
[`arc_open()`](https://developers.arcgis.com/r-bridge/reference/arc_open.md).
Optionally repair names using
[`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html).

## Usage

``` r
set_layer_aliases(
  .data,
  .layer,
  name_repair = "unique",
  alias = c("replace", "label"),
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

- name_repair:

  Default `"unique"`. See
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for details. If `name_repair = NULL` and `alias = "replace"` may
  include invalid names.

- alias:

  Use of field alias values. Defaults to `"replace"`. There are two
  options:

  - `"label"`: field alias values are assigned as a label attribute for
    each field.

  - `"replace"`: field alias values replace existing column names.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

A data.frame. When `alias = "replace"`, the column names are modified.
When `alias = "label"` each column has a new `label` attribute.

## Examples

``` r
furl <- paste0(
  "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/",
  "rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"
)

# open the feature service
flayer <- arc_open(furl)

# select first five rows
five_counties <- arc_select(flayer, n_max = 5)
#> Error: HTTP 504 Gateway Timeout.

# add aliases
with_aliases <- set_layer_aliases(five_counties, flayer)
#> Error: object 'five_counties' not found

# preview the new names
str(with_aliases, give.attr = FALSE)
#> Error: object 'with_aliases' not found
```
