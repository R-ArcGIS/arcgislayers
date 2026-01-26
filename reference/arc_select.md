# Query a Feature Service

`arc_select()` takes a `FeatureLayer`, `Table`, of `ImageServer` object
and returns data from the layer as an `sf` object or `data.frame`
respectively.

## Usage

``` r
arc_select(
  x,
  ...,
  fields = NULL,
  where = NULL,
  crs = sf::st_crs(x),
  geometry = TRUE,
  filter_geom = NULL,
  predicate = "intersects",
  n_max = Inf,
  page_size = NULL,
  token = arc_token()
)
```

## Arguments

- x:

  an object of class `FeatureLayer`, `Table`, or `ImageServer`.

- ...:

  additional query parameters passed to the API.

- fields:

  a character vector of the field names that you wish to be returned. By
  default all fields are returned.

- where:

  a simple SQL where statement indicating which features should be
  selected.

- crs:

  the spatial reference to be returned. If the CRS is different than the
  CRS for the input `FeatureLayer`, a transformation will occur
  server-side. Ignored if x is a `Table`.

- geometry:

  default `TRUE`. If geometries should be returned. Ignored for `Table`
  objects.

- filter_geom:

  an object of class `bbox`, `sfc` or `sfg` used to filter query results
  based on a predicate function.

- predicate:

  Spatial predicate to use with `filter_geom`. Default `"intersects"`.
  Possible options are `"intersects"`, `"contains"`, `"crosses"`,
  `"overlaps"`, `"touches"`, and `"within"`.

- n_max:

  the maximum number of features to return. By default returns every
  feature available. Unused at the moment.

- page_size:

  the maximum number of features to return per request. Useful when
  requests return a 500 error code. See Details.

- token:

  an `httr2_token` as created by `auth_code()` or similar

## Value

An sf object, or a data.frame

## Details

See [reference
documentation](https://developers.arcgis.com/rest/services-reference/enterprise/query-feature-service-layer-.htm#GUID-BC2AD141-3386-49FB-AA09-FF341145F614)
for possible arguments.

`FeatureLayers` can contain very dense geometries with a lot of
coordinates. In those cases, the feature service may time out before all
geometries can be returned. To address this issue, we can reduce the
number of features returned per each request by reducing the value of
the `page_size` parameter.

`arc_select()` works by sending a single request that counts the number
of features that will be returned by the current query. That number is
then used to calculate how many "pages" of responses are needed to fetch
all the results. The number of features returned (page size) is set to
the `maxRecordCount` property of the layer by default. However, by
setting `page_size` to be smaller than the `maxRecordCount` we can
return fewer geometries per page and avoid time outs.

**\[experimental\]**

## Examples

``` r
if (FALSE) { # \dontrun{
# define the feature layer url
furl <- paste0(
  "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest",
  "/services/PLACES_LocalData_for_BetterHealth/FeatureServer/0"
)

flayer <- arc_open(furl)

arc_select(
  flayer,
  fields = c("StateAbbr", "TotalPopulation")
)

arc_select(
  flayer,
  fields = c("OBJECTID", "PlaceName"),
  where = "TotalPopulation > 1000000"
)
} # }
```
