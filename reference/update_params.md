# Modify query parameters

`update_params()` takes named arguments and updates the query.

## Usage

``` r
update_params(x, ...)
```

## Arguments

- x:

  a `FeatureLayer` or `Table` object

- ...:

  key value pairs of query parameters and values.

## Value

An object of the same class as `x`

## Examples

``` r
if (FALSE) { # \dontrun{
furl <- paste0(
  "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/",
  "USA_Major_Cities_/FeatureServer/0"
)

flayer <- arc_open(furl)
update_params(flayer, outFields = "NAME")
} # }
```
