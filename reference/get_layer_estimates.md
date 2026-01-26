# Get Estimates

Get Estimates

## Usage

``` r
get_layer_estimates(x, token = arc_token())
```

## Arguments

- x:

  an object of class `FeatureLayer`, `Table`, or `ImageServer`.

- token:

  an `httr2_token` as created by `auth_code()` or similar

## Value

A named list containing all estimate info. If `extent` is present, it is
available as an object of class `bbox`.

## References

[ArcGIS REST
Doc](https://developers.arcgis.com/rest/services-reference/enterprise/get-estimates-feature-service-layer-.htm)

## Examples

``` r
if (FALSE) { # \dontrun{
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
furl <- paste0(
  "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/",
  "USA_Counties_Generalized_Boundaries/FeatureServer/0"
)

county_fl <- arc_open(furl)
get_layer_estimates(county_fl)
}
} # }
```
