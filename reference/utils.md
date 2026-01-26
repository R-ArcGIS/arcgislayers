# Utility functions

Utility functions

## Usage

``` r
clear_query(x)

list_fields(x)

pull_field_aliases(x)

list_items(x)

refresh_layer(x)
```

## Arguments

- x:

  an object of class `FeatureLayer`, `Table`, or `ImageServer`.

## Value

See Details.

## Details

**\[experimental\]**

- `list_fields()` returns a data.frame of the fields in a `FeatureLayer`
  or `Table`

- `list_items()` returns a data.frame containing the layers or tables in
  a `FeatureServer` or `MapServer`

- `clear_query()` removes any saved query in a `FeatureLayer` or `Table`
  object

- `refresh_layer()` syncs a `FeatureLayer` or `Table` with the remote
  resource picking up any changes that may have been made upstream.
  Returns an object of class `x`.

- `pull_field_aliases()` returns a named list of the field aliases from
  a `FeatureLayer` or `Table`

## Examples

``` r
if (FALSE) { # \dontrun{
furl <- paste0(
  "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/",
  "PLACES_LocalData_for_BetterHealth/FeatureServer/0"
)

flayer <- arc_open(furl)

# list fields available in a layer
list_fields(flayer)

# remove any queries stored in the query attribute
clear_query(update_params(flayer, outFields = "*"))

# refresh metadata of an object
refresh_layer(flayer)

map_url <- paste0(
  "https://services.arcgisonline.com/ArcGIS/rest/services/",
  "World_Imagery/MapServer"
)

# list all items in a server object
list_items(arc_open(map_url))
} # }
```
