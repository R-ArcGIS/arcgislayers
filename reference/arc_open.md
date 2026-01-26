# Access a Data Service or Portal Item

Access a resource on ArcGIS Online, Enterprise, or Location Platform.

## Usage

``` r
arc_open(url, host = arc_host(), token = arc_token())
```

## Arguments

- url:

  a url to a service such as a feature service, image server, or map
  server. Alternatively, an item ID of a portal item or portal url.

- host:

  default `"https://www.arcgis.com"`. The host of your ArcGIS Portal.

- token:

  an `httr2_token` as created by `auth_code()` or similar

## Value

Depending on item ID or URL returns a `PortalItem`, `FeatureLayer`,
`Table`, `FeatureServer`, `ImageServer`, or `MapServer`,
`GeocodeServer`, among other. Each of these objects is a named list
containing the properties of the service.

## Details

- To read the underlying attribute data from a `FeatureLayer`, `Table`,
  or `ImageServer` use
  [`arc_select()`](https://developers.arcgis.com/r-bridge/reference/arc_select.md).

- If you have a `MapServer` or `FeatureSever` access the individual
  layes using
  [`get_layer()`](https://developers.arcgis.com/r-bridge/reference/get_layer.md).
  For

- Use
  [`arc_raster()`](https://developers.arcgis.com/r-bridge/reference/arc_raster.md)
  to get imagery as a terra raster object.

**\[stable\]**

## See also

arc_select arc_raster get_layer

## Examples

``` r
if (FALSE) { # \dontrun{

# FeatureServer ID
arc_open("3b7221d4e47740cab9235b839fa55cd7")

# FeatureLayer
furl <- paste0(
  "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/",
  "PLACES_LocalData_for_BetterHealth/FeatureServer/0"
)

arc_open(furl)

# Table
furl <- paste0(
  "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/",
  "USA_Wetlands/FeatureServer/1"
)

arc_open(furl)

# ImageServer
arc_open(
  "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"
)

# FeatureServer
furl <- paste0(
  "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/",
  "PLACES_LocalData_for_BetterHealth/FeatureServer"
)

arc_open(furl)

# MapServer
map_url <- paste0(
  "https://services.arcgisonline.com/ArcGIS/rest/services/",
  "World_Imagery/MapServer"
)

arc_open(map_url)
} # }
```
