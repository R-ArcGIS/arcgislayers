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
#> ℹ Query results limited to 5 out of 3144 available features.

# add aliases
with_aliases <- set_layer_aliases(five_counties, flayer)

# preview the new names
str(with_aliases, give.attr = FALSE)
#> Classes ‘sf’ and 'data.frame':   5 obs. of  13 variables:
#>  $ OBJECTID              : num  1 2 3 4 5
#>  $ Name                  : chr  "Grand Forks County" "Grant County" "Griggs County" "Hettinger County" ...
#>  $ State Name            : chr  "North Dakota" "North Dakota" "North Dakota" "North Dakota" ...
#>  $ State FIPS            : chr  "38" "38" "38" "38" ...
#>  $ FIPS                  : chr  "38035" "38037" "38039" "38041" ...
#>  $ Area in square miles  : num  1440 1644 721 1131 1408
#>  $ 2020 Total Population : num  73170 2301 2306 2489 2394
#>  $ People per square mile: num  50.8 1.4 3.2 2.2 1.7
#>  $ State Abbreviation    : chr  "ND" "ND" "ND" "ND" ...
#>  $ County FIPS           : chr  "035" "037" "039" "041" ...
#>  $ Shape__Area           : num  0.45 0.504 0.223 0.343 0.438
#>  $ Shape__Length         : num  2.96 3.41 1.95 2.69 2.72
#>  $ geometry              :sfc_POLYGON of length 5; first list element: List of 1
#>   ..$ : num [1:26, 1:2] -96.9 -97.5 -97.9 -97.9 -97.9 ...
```
