# Add, update, or delete a Feature Layer definition

Each layer of a feature service is defined by a "definition." The
definition describes the service such as its fields, symbology, indexes
and more.

## Usage

``` r
add_layer_definition(x, ..., async = FALSE, token = arc_token())

update_layer_definition(x, ..., async = FALSE, token = arc_token())

delete_layer_definition(x, ..., async = FALSE, token = arc_token())
```

## Arguments

- x:

  A Feature Layer, Table, or Feature Service class object.

- ...:

  Additional parameters for the "addToDefinition" or "updateDefinition"
  body of the request.

- async:

  Default `FALSE`. If `TRUE`, support asynchronous processing for the
  request.

- token:

  an `httr2_token` as created by `auth_code()` or similar

## Value

If `async = FALSE`, return an updated "FeatureServer" or "FeatureLayer"
object with the added, updated, or deleted definitions. If
`async = TRUE`, the input Feature Layer or Feature Server object `x` is
returned as is.

## Details

**\[experimental\]**

- Use `add_layer_definition()` for adding fields to a feature service or
  otherwise adding to the definition of a feature layer.

- Use `update_layer_definition()` to modify existing aspects of the
  definition properties.

- Use `delete_layer_definition()` to delete properties from the layer
  definition.

Examples of properties include the layer name, renderer, or field
properties. Named parameters passed to `...` must have names matching
supported definitions. Parameters are converted to a JSON
`addToDefinition`, `updateDefinition`, or `deleteFromDefinition` query
parameter using
[`jsonify::to_json()`](https://symbolixau.github.io/jsonify/reference/to_json.html).

See the ArcGIS REST API documentation on Administer Hosted Feature
Services for more details:

- see the
  [layerDefinition](https://developers.arcgis.com/web-map-specification/objects/layerDefinition/)
  object documentation.

- adding definitions for a
  [FeatureLayer](https://developers.arcgis.com/rest/services-reference/online/add-to-definition-feature-layer/)
  or [a
  FeatureService](https://developers.arcgis.com/rest/services-reference/online/add-to-definition-feature-service/)

- updating definitions for [a
  FeatureLayer](https://developers.arcgis.com/rest/services-reference/online/update-definition-feature-layer/)
  or [a
  FeatureService](https://developers.arcgis.com/rest/services-reference/online/update-definition-feature-service-.htm)

- deleting definitions for [a
  FeatureLayer](https://developers.arcgis.com/rest/services-reference/online/delete-from-definition-feature-layer/)
  or a
  [FeatureService](https://developers.arcgis.com/rest/services-reference/online/delete-from-definition-feature-service/)

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive()) {
# authenticate
set_arc_token(auth_code())

# publish a layer
published <- publish_layer(penguins, "Penguin Test")

penguin_fl <- arc_open(published$services$encodedServiceURL) |>
  get_layer(0)

# Update the name of the layer
update_layer_definition(
  penguin_fl,
  name = "New Layer Name"
)

# add an index on the the layer
add_layer_definition(
  penguin_fl,
  indexes = list(
    name = "index1",
    fields = "species",
    isUnique = FALSE,
    isAscending = FALSE,
    description = "Example index"
  )
)

# refresh the layer to get the updates
penguin_fl <- refresh_layer(penguin_fl)
penguin_fl[["indexes"]]
}
} # }
```
