# Publish Content

Publishes an `sf` or `data.frame` object to an ArcGIS Portal as a
FeatureCollection.

## Usage

``` r
add_item(
  x,
  title,
  description = "",
  tags = character(0),
  snippet = "",
  categories = character(0),
  async = FALSE,
  type = "Feature Service",
  token = arc_token()
)

publish_item(
  item_id,
  publish_params = .publish_params(),
  file_type = "featureCollection",
  token = arc_token()
)

publish_layer(
  x,
  title,
  ...,
  publish_params = .publish_params(title, target_crs = sf::st_crs(x)),
  token = arc_token()
)

.publish_params(
  name = NULL,
  description = NULL,
  copyright = NULL,
  target_crs = 3857,
  max_record_count = 2000L
)
```

## Arguments

- x:

  an object of class `data.frame`. This can be an `sf` object or
  `tibble` or any other subclass of `data.frame`.

- title:

  A user-friendly string title for the layer that can be used in a table
  of contents.

- description:

  a length 1 character vector containing the description of the item
  that is being added. Note that the value cannot be larger than 64kb.

- tags:

  a character vector of tags to add to the item.

- snippet:

  a length 1 character vector with no more than 2048 characters.

- categories:

  a character vector of the categories of the item.

- async:

  default `FALSE`. Cannot be changed at this time.

- type:

  default `"Feature Service"`. Must not be changed at this time.

- token:

  an `httr2_token` as created by `auth_code()` or similar

- item_id:

  the ID of the item to be published.

- publish_params:

  a list of named values of the `publishParameters`. Must match the
  values in the [/publish endpoint
  documentation](https://developers.arcgis.com/rest/users-groups-and-items/publish-item.htm#GUID-9E8F8526-5D58-4706-95F3-432905CC3303).

- file_type:

  default `"featureCollection"`. Cannot be changed.

- ...:

  arguments passed into `add_item()`.

- name:

  a scalar character of the name of the layer. Must be unique.

- copyright:

  an optional character scalar containing copyright text to add to the
  published Feature Service.

- target_crs:

  the CRS of the Feature Service to be created. By default, `EPSG:3857`.

- max_record_count:

  the maximum number of records that can be returned from the created
  Feature Service.

## Value

A named list containing the url of the newly published service.

## Details

**\[experimental\]**

- `add_item()` takes a data.frame like object and uploads it as an item
  in your portal.

- `publish_item()` takes an ID of an item in your portal and publishes
  it as a feature service.

- `publish_layer()` is a high-level wrapper that first adds an object as
  an item in your portal and subsequently publishes it for you.

- `.publish_params()` is a utility function to specify optional publish
  parameters such as copyright text, and the spatial reference of the
  published feature collection.

Note that there is *only* support for feature services meaning that only
tables and feature layers can be made by these functions.

### Publish Parameters

When publishing an item to a portal, a number of [publish
parameters](https://developers.arcgis.com/rest/users-groups-and-items/publish-item.htm#GUID-9E8F8526-5D58-4706-95F3-432905CC3303)
can be provided. Most importantly is the `targetSR` which will be the
CRS of the hosted feature service. By default this is `EPSG:3857`.

`publish_layer()` will use the CRS of the input object, `x`, by default.
If publishing content in two steps with `add_item()` and
`publish_item()`, use `.publish_params()` to craft your publish
parameters. Ensure that the CRS provided to `target_crs` matches that of
the item you added with `add_item()`.

## Examples

``` r
if (FALSE) { # \dontrun{
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  x <- nc[1:5, 13]

  token <- auth_code()
  set_arc_token(token)

  publish_res <- publish_layer(
    x, "North Carolina SIDS sample"
  )
} # }
```
