
<!-- README.md is generated from README.Rmd. Please edit that file -->

# arcgis

<!-- badges: start -->
<!-- badges: end -->

The goal of arcgis is to provide an R interface to the ArcGIS REST API
and a completely open source analogue to the ArcGIS Python API.

## Installation

You can install the development version of arcgis like so:

You need to be connected to the VPN.

``` r
remotes::install_git("https://devtopia.esri.com/jos13045/arcgis")
```

## Feature Layers

Read in data from a feature layer. arcgis implements a `FeatureLayer`
class object which is used to control how we read data from a Feature
Layer service.

Create a `FeatureLayer` with `feature_layer()` which takes the url of
the feature layer as its only argument.

``` r
library(arcgis)
#> 
#> Attaching package: 'arcgis'
#> The following object is masked from 'package:stats':
#> 
#>     filter

# define the feature layer url
furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

# create a feature layer
county_fl <- feature_layer(furl)

county_fl
#> <FeatureLayer <3143 features, 12 fields>>
#> Name: USA Counties - Generalized
#> Geometry Type: esriGeometryPolygon
#> CRS: 4326
#> Capabilities: Query,Extract
```

`FeatureLayer` objects are lists that contain metadata from the
FeatureServer. The data that is used to create the `FeatureLayer` is
taken directly from the url when displayed in json format.
`FeatureLayer`s are intended to work somewhat similarly to a lazy table
where data is only brought into memory when explicitly requested.

The `collect()` function will fetch data from the REST endpoint and
write it to the appropriate type—either an sf object or a data frame.

``` r
county_sf <- collect(county_fl)

county_sf
#> Simple feature collection with 3142 features and 12 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -178.2176 ymin: 18.92179 xmax: -66.96927 ymax: 71.40624
#> Geodetic CRS:  WGS 84
#> # A tibble: 3,142 × 13
#>    OBJECTID NAME     STATE…¹ STATE…² FIPS   SQMI POPUL…³ POP_S…⁴ STATE…⁵ COUNT…⁶
#>  *    <int> <chr>    <chr>   <chr>   <chr> <dbl>   <int>   <dbl> <chr>   <chr>  
#>  1        1 Autauga… Alabama 01      01001  604.   58805    97.3 AL      001    
#>  2        2 Baldwin… Alabama 01      01003 1633.  231767   142.  AL      003    
#>  3        3 Barbour… Alabama 01      01005  905.   25223    27.9 AL      005    
#>  4        4 Bibb Co… Alabama 01      01007  626.   22293    35.6 AL      007    
#>  5        5 Blount … Alabama 01      01009  651.   59134    90.9 AL      009    
#>  6        6 Bullock… Alabama 01      01011  625.   10357    16.6 AL      011    
#>  7        7 Butler … Alabama 01      01013  778.   19051    24.5 AL      013    
#>  8        8 Calhoun… Alabama 01      01015  612.  116441   190.  AL      015    
#>  9        9 Chamber… Alabama 01      01017  603.   34772    57.7 AL      017    
#> 10       10 Cheroke… Alabama 01      01019  600.   24971    41.6 AL      019    
#> # … with 3,132 more rows, 3 more variables: Shape__Area <dbl>,
#> #   Shape__Length <dbl>, geometry <MULTIPOLYGON [°]>, and abbreviated variable
#> #   names ¹​STATE_NAME, ²​STATE_FIPS, ³​POPULATION, ⁴​POP_SQMI, ⁵​STATE_ABBR,
#> #   ⁶​COUNTY_FIPS
```

We can check to see what fields and types are present in our feature
layer using `list_fields()`.

``` r
list_fields(county_fl)
#> # A tibble: 12 × 10
#>    name        type  alias sqlType nulla…¹ edita…² domain defau…³ length descr…⁴
#>    <chr>       <chr> <chr> <chr>   <lgl>   <lgl>   <lgl>  <lgl>    <int> <chr>  
#>  1 OBJECTID    esri… OBJE… sqlTyp… FALSE   FALSE   NA     NA          NA  <NA>  
#>  2 NAME        esri… Name  sqlTyp… TRUE    TRUE    NA     NA          50 "{\"va…
#>  3 STATE_NAME  esri… Stat… sqlTyp… TRUE    TRUE    NA     NA          20 "{\"va…
#>  4 STATE_FIPS  esri… Stat… sqlTyp… TRUE    TRUE    NA     NA           2 "{\"va…
#>  5 FIPS        esri… FIPS  sqlTyp… TRUE    TRUE    NA     NA           5 "{\"va…
#>  6 SQMI        esri… Area… sqlTyp… TRUE    TRUE    NA     NA          NA "{\"va…
#>  7 POPULATION  esri… 2020… sqlTyp… TRUE    TRUE    NA     NA          NA "{\"va…
#>  8 POP_SQMI    esri… Peop… sqlTyp… TRUE    TRUE    NA     NA          NA "{\"va…
#>  9 STATE_ABBR  esri… Stat… sqlTyp… TRUE    TRUE    NA     NA           2 "{\"va…
#> 10 COUNTY_FIPS esri… Coun… sqlTyp… TRUE    TRUE    NA     NA           3 "{\"va…
#> 11 Shape__Area esri… Shap… sqlTyp… TRUE    FALSE   NA     NA          NA  <NA>  
#> 12 Shape__Len… esri… Shap… sqlTyp… TRUE    FALSE   NA     NA          NA  <NA>  
#> # … with abbreviated variable names ¹​nullable, ²​editable, ³​defaultValue,
#> #   ⁴​description
```

### dplyr-like syntax

You may not need all of these fields or features in your R session. If
that’s the case, it is more effective to bring in only a subset of the
data that you need. We can limit what is brought into memory by using
`select()` and `filter()` functions to build up a query that can be sent
the rest API.

The `select()` `FeatureLayer` method supports using `{tidyselect}`
functions except the `where()` function.

``` r
county_fl |> 
  select(contains("STATE"))
#> <FeatureLayer <3143 features, 12 fields>>
#> Name: USA Counties - Generalized
#> Geometry Type: esriGeometryPolygon
#> CRS: 4326
#> Capabilities: Query,Extract
#> 
#> ── Query
#> outFields: STATE_NAME,STATE_FIPS,STATE_ABBR
```

Now that we’ve ran `select()` on the `FeatureLayer` query parameters are
printed out. The parameters will be sent along when the results are
`collect()`ed. 

You can further subset your data by using `filter()`. Note that
filtering is fairly limited ([see API documentation for
more](https://developers.arcgis.com/rest/services-reference/enterprise/query-feature-service-layer-.htm#GUID-BC2AD141-3386-49FB-AA09-FF341145F614)).

``` r
county_query <- county_fl |> 
  select(starts_with("STATE")) |> 
  filter(STATE_NAME == "Alabama")

county_query
#> <FeatureLayer <3143 features, 12 fields>>
#> Name: USA Counties - Generalized
#> Geometry Type: esriGeometryPolygon
#> CRS: 4326
#> Capabilities: Query,Extract
#> 
#> ── Query
#> outFields: STATE_NAME,STATE_FIPS,STATE_ABBR
#> where: STATE_NAME = 'Alabama'
```

Bring it into memory with `collect()`.

``` r
res <- collect(county_query)
res
#> Simple feature collection with 67 features and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -88.47295 ymin: 30.2336 xmax: -84.89402 ymax: 35.01603
#> Geodetic CRS:  WGS 84
#> # A tibble: 67 × 4
#>    STATE_NAME STATE_FIPS STATE_ABBR                                     geometry
#>  * <chr>      <chr>      <chr>                                     <POLYGON [°]>
#>  1 Alabama    01         AL         ((-86.82067 32.34731, -86.81446 32.37041, -…
#>  2 Alabama    01         AL         ((-87.97309 31.16482, -87.9371 31.17346, -8…
#>  3 Alabama    01         AL         ((-85.74337 31.62624, -85.7172 31.67924, -8…
#>  4 Alabama    01         AL         ((-87.41986 33.01177, -87.31532 33.0121, -8…
#>  5 Alabama    01         AL         ((-86.96799 33.86045, -86.92667 33.87228, -…
#>  6 Alabama    01         AL         ((-85.4114 32.15551, -85.41136 32.14547, -8…
#>  7 Alabama    01         AL         ((-86.44912 31.97123, -86.45245 31.85498, -…
#>  8 Alabama    01         AL         ((-85.79353 33.56343, -85.79315 33.59482, -…
#>  9 Alabama    01         AL         ((-85.58963 32.73135, -85.59478 33.11425, -…
#> 10 Alabama    01         AL         ((-85.41657 34.08692, -85.39574 33.95983, -…
#> # … with 57 more rows
```

### Other `FeatureLayer` functions

You can add any parameter that you’d like that is supported by the query
parameters [listed in the
documentation](https://developers.arcgis.com/rest/services-reference/enterprise/query-feature-service-layer-.htm#GUID-BC2AD141-3386-49FB-AA09-FF341145F614)
with the using the `update_params()` function.

``` r
county_query |> 
  update_params(returnIdsOnly = "true") |> 
  collect()
#>      objectIdFieldName objectIds 
#> [1,] "OBJECTID"        integer,67
```

Maybe, you want to clear our your query parameters, use `clear_query()`.

``` r
clear_query(county_query)
#> <FeatureLayer <3143 features, 12 fields>>
#> Name: USA Counties - Generalized
#> Geometry Type: esriGeometryPolygon
#> CRS: 4326
#> Capabilities: Query,Extract
```

To preview only a few features from the feature layer use `head()`

``` r
head(county_query)
#> Simple feature collection with 6 features and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -88.02045 ymin: 30.2336 xmax: -85.05665 ymax: 34.26583
#> Geodetic CRS:  WGS 84
#> # A tibble: 6 × 4
#>   STATE_NAME STATE_FIPS STATE_ABBR                                      geometry
#>   <chr>      <chr>      <chr>                                      <POLYGON [°]>
#> 1 Alabama    01         AL         ((-86.82067 32.34731, -86.81446 32.37041, -8…
#> 2 Alabama    01         AL         ((-87.97309 31.16482, -87.9371 31.17346, -87…
#> 3 Alabama    01         AL         ((-85.74337 31.62624, -85.7172 31.67924, -85…
#> 4 Alabama    01         AL         ((-87.41986 33.01177, -87.31532 33.0121, -87…
#> 5 Alabama    01         AL         ((-86.96799 33.86045, -86.92667 33.87228, -8…
#> 6 Alabama    01         AL         ((-85.4114 32.15551, -85.41136 32.14547, -85…
```

## `FeatureServers`

FeatureServers contain one or more `FeatureLayers` or `Tables`.

``` r
ft_srv <- feature_server(
  "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/List_of_Providers/FeatureServer/"
  )

ft_srv
#> <FeatureServer <2 Features>>
#> CRS: 3857
#> Capabilities: Query
#> ┌ Features ──────────────────────────────────────┐
#> │ 24: Adoption_Facilities (esriGeometryPolygon)  │
#> │ 27: Adoption_Org (Table)                       │
#> └────── List of adoption providers in each state ┘
```

Access elements by ID with `get_layer()`

``` r
get_layer(ft_srv, 27)
#> <Table <281 features, 8 fields>>
#> Name: Adoption_Org
#> Capabilities: Query
```

Get a list of all elements.

``` r
get_all_layers(ft_srv)
#> $layers
#> $layers$`24`
#> <FeatureLayer <51 features, 9 fields>>
#> Name: Adoption_Facilities
#> Geometry Type: esriGeometryPolygon
#> CRS: 3857
#> Capabilities: Query
#> 
#> 
#> $tables
#> $tables$`27`
#> <Table <281 features, 8 fields>>
#> Name: Adoption_Org
#> Capabilities: Query
```

## Conversion to Esri JSON

sf objects can be converted to Esri JSON representation. These can be
Geometry Objects, FeatureSets, or arrays of Features.

`st_as_geometry()` works with objects of class `sfg` and creates a
Geometry Object. `st_as_featureset()` creates a FeatureSet from an sf or
sfc object. sfc objects will have 0 length attributes.
`st_as_features()` works with an sf or sfc object.

``` r
st_as_geometry(res[["geometry"]][[30]]) |> 
  # print nicely with jsonify
  jsonify::pretty_json()
#> Registered S3 method overwritten by 'jsonify':
#>   method     from    
#>   print.json jsonlite
#> {
#>     "hasZ": false,
#>     "hasM": false,
#>     "rings": [
#>         [
#>             [
#>                 -87.5341266760964,
#>                 34.3133458024783
#>             ],
#>             [
#>                 -87.6337314942754,
#>                 34.3127157099462
#>             ],
#>             [
#>                 -88.1676133052101,
#>                 34.3241475145521
#>             ],
#>             [
#>                 -88.151256386964,
#>                 34.4652733221692
#>             ],
#>             [
#>                 -88.1364015786465,
#>                 34.580497317268
#>             ],
#>             [
#>                 -87.5304598825747,
#>                 34.5677579787062
#>             ],
#>             [
#>                 -87.5341266760964,
#>                 34.3133458024783
#>             ]
#>         ]
#>     ],
#>     "spatialReference": {
#>         "wkid": 4326
#>     }
#> }
```

``` r
st_as_featureset(res[30,]) |> 
  jsonify::pretty_json()
#> {
#>     "geometryType": "esriGeometryPolygon",
#>     "spatialReference": {
#>         "wkid": 4326
#>     },
#>     "hasZ": false,
#>     "hasM": false,
#>     "features": [
#>         {
#>             "attributes": {
#>                 "STATE_NAME": "Alabama",
#>                 "STATE_FIPS": "01",
#>                 "STATE_ABBR": "AL"
#>             },
#>             "geometry": {
#>                 "rings": [
#>                     [
#>                         [
#>                             -87.5341266760964,
#>                             34.3133458024783
#>                         ],
#>                         [
#>                             -87.6337314942754,
#>                             34.3127157099462
#>                         ],
#>                         [
#>                             -88.1676133052101,
#>                             34.3241475145521
#>                         ],
#>                         [
#>                             -88.151256386964,
#>                             34.4652733221692
#>                         ],
#>                         [
#>                             -88.1364015786465,
#>                             34.580497317268
#>                         ],
#>                         [
#>                             -87.5304598825747,
#>                             34.5677579787062
#>                         ],
#>                         [
#>                             -87.5341266760964,
#>                             34.3133458024783
#>                         ]
#>                     ]
#>                 ]
#>             }
#>         }
#>     ]
#> }
```
