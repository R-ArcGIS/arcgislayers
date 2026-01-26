# Truncate a Feature Layer

Removes all features in a Feature Layer or Table and resets the object
ID counter. Truncating a Feature Layer does not change the schema of the
data (does not add, remove, or alter existing database columns,
constraints, or indexes).

## Usage

``` r
truncate_layer(x, async = FALSE, attachment_only = FALSE, token = arc_token())
```

## Arguments

- x:

  an object of class `FeatureLayer`, `Table`, or `ImageServer`.

- async:

  default `FALSE`. It is recommended to set `TRUE` for larger datasets.

- attachment_only:

  default `FALSE`. Deletes all the attachments for this layer. None of
  the layer features will be deleted when `TRUE`.

- token:

  an `httr2_token` as created by `auth_code()` or similar

## Value

a named list with the name "success" and a value of `TRUE` or `FALSE`

## References

[ArcGIS Developers Rest API
Doc](https://developers.arcgis.com/rest/services-reference/online/truncate-feature-layer-.htm)

## Examples

``` r
if (FALSE) { # \dontrun{

  # authorize using code flow
  set_arc_token(auth_code())

  # create a FeatureLayer object
  flayer <- arc_open("your-feature-layer-url")

  # truncate it
  truncate_layer(flayer)
} # }
```
