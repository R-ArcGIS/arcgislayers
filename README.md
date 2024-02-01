
<!-- README.md is generated from README.Rmd. Please edit that file -->

# arcgislayers

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![R-CMD-check](https://github.com/R-ArcGIS/arcgislayers/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/R-ArcGIS/arcgislayers/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `{arcgislayers}` is to provide an R interface to the ArcGIS
REST API

## Installation

arcgislayers requires arcgisutils for authorization. It is recommend you
install and use the metapackage `{arcgis}`. You can install the
development version of arcgis like so:

``` r
remotes::install_github("r-arcgis/arcgis", dependencies = TRUE)
```

## Basic usage

``` r
library(arcgis)
#> Attaching core arcgis packages:
#>   - {arcgisutils} v0.1.1.9001
#>   - {arcgislayers} v0.1.0
```

`arc_open()` takes a URL to create a reference to a remote resource. The
function can return any of four classes (corresponding to different
ArcGIS service types):

- `FeatureLayer`
- `Table`
- `FeatureServer`
- `ImageServer`

For example, you can create a `FeatureLayer` object based on a Feature
Server URL:

``` r
furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

county_fl <- arc_open(furl)

county_fl
#> <FeatureLayer>
#> Name: USA Counties - Generalized
#> Geometry Type: esriGeometryPolygon
#> CRS: 4326
#> Capabilities: Query,Extract
```

You can then query the feature layer object and return an `sf` object
using `arc_select()`.

If no arguments are provided to `arc_select()` the entire feature layer
is returned in memory as an `sf` object.

``` r
arc_select(county_fl)
#> Simple feature collection with 3142 features and 12 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -178.2176 ymin: 18.92179 xmax: -66.96927 ymax: 71.40624
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>    OBJECTID            NAME STATE_NAME STATE_FIPS  FIPS    SQMI POPULATION
#> 1         1  Autauga County    Alabama         01 01001  604.37      58805
#> 2         2  Baldwin County    Alabama         01 01003 1633.14     231767
#> 3         3  Barbour County    Alabama         01 01005  904.52      25223
#> 4         4     Bibb County    Alabama         01 01007  626.17      22293
#> 5         5   Blount County    Alabama         01 01009  650.63      59134
#> 6         6  Bullock County    Alabama         01 01011  625.14      10357
#> 7         7   Butler County    Alabama         01 01013  777.88      19051
#> 8         8  Calhoun County    Alabama         01 01015  612.27     116441
#> 9         9 Chambers County    Alabama         01 01017  603.11      34772
#> 10       10 Cherokee County    Alabama         01 01019  599.98      24971
#>    POP_SQMI STATE_ABBR COUNTY_FIPS Shape__Area Shape__Length
#> 1      97.3         AL         001   0.1489034      1.884137
#> 2     141.9         AL         003   0.4044891      3.678276
#> 3      27.9         AL         005   0.2224307      2.218514
#> 4      35.6         AL         007   0.1577359      1.852453
#> 5      90.9         AL         009   0.1675296      2.067456
#> 6      16.6         AL         011   0.1557273      2.006250
#> 7      24.5         AL         013   0.1927305      1.769462
#> 8     190.2         AL         015   0.1523369      2.149825
#> 9      57.7         AL         017   0.1531136      1.637226
#> 10     41.6         AL         019   0.1527217      1.794142
#>                          geometry
#> 1  MULTIPOLYGON (((-86.82067 3...
#> 2  MULTIPOLYGON (((-87.97309 3...
#> 3  MULTIPOLYGON (((-85.74337 3...
#> 4  MULTIPOLYGON (((-87.41986 3...
#> 5  MULTIPOLYGON (((-86.96799 3...
#> 6  MULTIPOLYGON (((-85.4114 32...
#> 7  MULTIPOLYGON (((-86.44912 3...
#> 8  MULTIPOLYGON (((-85.79353 3...
#> 9  MULTIPOLYGON (((-85.58963 3...
#> 10 MULTIPOLYGON (((-85.41657 3...
```

You can also use the `fields` argument to subset columns or `where`
argument to subset rows.

For example, using a character vector of column names for `fields` and a
simple SQL where clause for `where` you can select counties with
population greater than 1,000,000:

``` r
arc_select(
  county_fl, 
  fields = c("state_abbr", "population"), 
  where = "population > 1000000"
)
#> Simple feature collection with 49 features and 2 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -158.2674 ymin: 21.24986 xmax: -71.02671 ymax: 47.77552
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>    STATE_ABBR POPULATION                       geometry
#> 1          AZ    4420568 MULTIPOLYGON (((-111.0425 3...
#> 2          AZ    1043433 MULTIPOLYGON (((-110.4522 3...
#> 3          CA    1682353 MULTIPOLYGON (((-121.4721 3...
#> 4          CA    1165927 MULTIPOLYGON (((-122.3076 3...
#> 5          CA    1008654 MULTIPOLYGON (((-120.6636 3...
#> 6          CA   10014009 MULTIPOLYGON (((-118.1067 3...
#> 7          CA    3186989 MULTIPOLYGON (((-117.509 33...
#> 8          CA    2418185 MULTIPOLYGON (((-116.0824 3...
#> 9          CA    1585055 MULTIPOLYGON (((-121.6652 3...
#> 10         CA    2181654 MULTIPOLYGON (((-117.7832 3...
```

The `list_fields()` function can be helpful to check available
attributes and build a `where` query:

``` r
list_fields(county_fl)
#>             name                 type                  alias       sqlType
#> 1       OBJECTID     esriFieldTypeOID               OBJECTID  sqlTypeOther
#> 2           NAME  esriFieldTypeString                   Name  sqlTypeOther
#> 3     STATE_NAME  esriFieldTypeString             State Name  sqlTypeOther
#> 4     STATE_FIPS  esriFieldTypeString             State FIPS  sqlTypeOther
#> 5           FIPS  esriFieldTypeString                   FIPS  sqlTypeOther
#> 6           SQMI  esriFieldTypeDouble   Area in square miles  sqlTypeOther
#> 7     POPULATION esriFieldTypeInteger  2020 Total Population  sqlTypeOther
#> 8       POP_SQMI  esriFieldTypeDouble People per square mile  sqlTypeOther
#> 9     STATE_ABBR  esriFieldTypeString     State Abbreviation  sqlTypeOther
#> 10   COUNTY_FIPS  esriFieldTypeString            County FIPS  sqlTypeOther
#> 11   Shape__Area  esriFieldTypeDouble            Shape__Area sqlTypeDouble
#> 12 Shape__Length  esriFieldTypeDouble          Shape__Length sqlTypeDouble
#>    nullable editable domain defaultValue length
#> 1     FALSE    FALSE     NA           NA     NA
#> 2      TRUE     TRUE     NA           NA     50
#> 3      TRUE     TRUE     NA           NA     20
#> 4      TRUE     TRUE     NA           NA      2
#> 5      TRUE     TRUE     NA           NA      5
#> 6      TRUE     TRUE     NA           NA     NA
#> 7      TRUE     TRUE     NA           NA     NA
#> 8      TRUE     TRUE     NA           NA     NA
#> 9      TRUE     TRUE     NA           NA      2
#> 10     TRUE     TRUE     NA           NA      3
#> 11     TRUE    FALSE     NA           NA     NA
#> 12     TRUE    FALSE     NA           NA     NA
#>                                                                                                                                                                                                                 description
#> 1                                                                                                                                                                                                                      <NA>
#> 2                                                                                                                                                        {"value":"The name of the county.","fieldValueType":"nameOrTitle"}
#> 3                                                                                                                         {"value":"The name for the state in which the county is located.","fieldValueType":"nameOrTitle"}
#> 4                                                                                                 {"value":"The code (two-digit number) for the state in which the county is located.","fieldValueType":"uniqueIdentifier"}
#> 5  {"value":"The combined state and county codes. County codes begin with 001 for each state; use the combined code (five-digit number) to uniquely identify a county in the country.","fieldValueType":"uniqueIdentifier"}
#> 6                                                                             {"value":"The area of the county in square miles using the North America Albers Equal Area Conic projection.","fieldValueType":"measurement"}
#> 7                                                                                                                                           {"value":"The 2020 population of the county.","fieldValueType":"countOrAmount"}
#> 8                                                                                                                             {"value":"The 2020 population of the county per square mile.","fieldValueType":"measurement"}
#> 9                                                                                                 {"value":"The two-letter abbreviation for the state in which the county is located.","fieldValueType":"uniqueIdentifier"}
#> 10                                                                                                                            {"value":"The code (three-digit number) for the county.","fieldValueType":"uniqueIdentifier"}
#> 11                                                                                                                                                                                                                     <NA>
#> 12                                                                                                                                                                                                                     <NA>
```

You can also provide a `bbox`, `sfc`, or `sfg` object to the
`filter_geom` argument to perform a spatial filter. If the `sfc` object
contains more than one geometry, the object is combined with
`sf::st_union()`. See documentation for more (`?arc_select`).

``` r
nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#> Reading layer `nc' from data source 
#>   `/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/library/sf/shape/nc.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27

arc_select(
  county_fl,
  filter_geom = sf::st_bbox(nc[1,])
)
#> Simple feature collection with 6 features and 12 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -82.0477 ymin: 35.98946 xmax: -80.83795 ymax: 36.80746
#> Geodetic CRS:  WGS 84
#>   OBJECTID             NAME     STATE_NAME STATE_FIPS  FIPS   SQMI POPULATION
#> 1     1890 Alleghany County North Carolina         37 37005 236.26      10888
#> 2     1892      Ashe County North Carolina         37 37009 429.38      26577
#> 3     1982   Watauga County North Carolina         37 37189 313.32      54086
#> 4     1984    Wilkes County North Carolina         37 37193 756.33      65969
#> 5     2471   Johnson County      Tennessee         47 47091 302.69      17948
#> 6     2855   Grayson County       Virginia         51 51077 445.57      15333
#>   POP_SQMI STATE_ABBR COUNTY_FIPS Shape__Area Shape__Length
#> 1     46.1         NC         005  0.06140165      1.231232
#> 2     61.9         NC         009  0.11428581      1.442112
#> 3    172.6         NC         189  0.08142272      1.287674
#> 4     87.2         NC         193  0.19911944      1.984232
#> 5     59.3         TN         091  0.07960385      1.290607
#> 6     34.4         VA         077  0.11578917      1.945424
#>                         geometry
#> 1 MULTIPOLYGON (((-81.2397 36...
#> 2 MULTIPOLYGON (((-81.47258 3...
#> 3 MULTIPOLYGON (((-81.80605 3...
#> 4 MULTIPOLYGON (((-81.02037 3...
#> 5 MULTIPOLYGON (((-81.74091 3...
#> 6 MULTIPOLYGON (((-81.34512 3...
```

A `SpatRaster` object from the `{terra}` package can be extracted from
an `ImageServer` using `arc_raster()`.

`arc_raster()` requires a `bbox` class object from the `sf` package to
define the area to be extracted. You can optionally specify the `width`
and `height` of the resultant image. Use `format` to define what type of
image is returned.

``` r
img_url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"

landsat <- arc_open(img_url)

res <- arc_raster(
  landsat, 
  xmin = -71, ymin = 43, 
  xmax = -67, ymax = 47.5, 
  bbox_crs = 4326, 
  width = 500, height = 500
)

terra::plotRGB(res, 4, 3, 2, scale = max(landsat[["maxValues"]]))
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

## Authorization

See [authorization article](articles/Authorization.html).

------------------------------------------------------------------------

<!-- # DEPRECATED FROM HERE DOWN  -->
<!-- TODO update and rewrite -->
<!-- ## Feature Layers -->
<!-- Read in data from a feature layer. arcgis implements a `FeatureLayer` class object which is used to control how we read data from a Feature Layer service.  -->
<!-- Create a `FeatureLayer` with `feature_layer()` which takes the url of the feature layer as its only argument.  -->
<!-- ```{r example} -->
<!-- library(arcgis) -->
<!-- # define the feature layer url -->
<!-- furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0" -->
<!-- # create a feature layer -->
<!-- county_fl <- feature_layer(furl) -->
<!-- county_fl -->
<!-- ``` -->
<!-- `FeatureLayer` objects are lists that contain metadata from the FeatureServer. The data that is used to create the `FeatureLayer` is taken directly from the url when displayed in json format. `FeatureLayer`s are intended to work somewhat similarly to a lazy table where data is only brought into memory when explicitly requested.  -->
<!-- The `collect_layer()` function will fetch all data for a layer from the REST endpoint and write it to the appropriate type—either an sf object or a data frame.  -->
<!-- ```{r cars} -->
<!-- county_sf <- collect_layer(county_fl) -->
<!-- county_sf -->
<!-- ``` -->
<!-- We can check to see what fields and types are present in our feature layer using `list_fields()`.  -->
<!-- ```{r} -->
<!-- list_fields(county_fl) -->
<!-- ``` -->
<!-- You can provide a more detailed query to the layer using `query_layer()`.  -->
<!-- ```{r} -->
<!-- res <- query_layer( -->
<!--   county_fl,  -->
<!--   where = "STATE_NAME = 'California' and POPULATION >= 30000", -->
<!--   fields = c("FIPS", "STATE_NAME", "SQMI", "POPULATION") -->
<!-- ) -->
<!-- res -->
<!-- ``` -->
<!-- Additionally, you can provide sf objects as spatial filters based on a predicate function.  Possible options are "intersects", "contains", "crosses", "overlaps", "touches", and "within". -->
<!-- In this example we create a polygon representing the bounding box of the West Coast of the US to act as our spatial filter.  -->
<!-- ```{r} -->
<!-- west_coast_bbox <- sf::st_as_sfc( -->
<!--   sf::st_bbox(c(xmin = -125, ymin = 32, xmax = -114, ymax = 49)) -->
<!-- ) -->
<!-- west_coast_bbox -->
<!-- ``` -->
<!-- We can then provide it as input to the `filter_geom` argument to act as a spatial filter.  -->
<!-- ```{r} -->
<!-- res <- query_layer( -->
<!--   county_fl,  -->
<!--   filter_geom = west_coast_bbox -->
<!-- ) -->
<!-- plot(res$geometry) -->
<!-- plot(west_coast_bbox, lty = 3, add = TRUE) -->
<!-- ``` -->
<!-- For manual control of the parameters that will be sent to the REST API you can use `update_params()`. This function allows you to built up your query iteratively and programmatically without executing the query right away. Use it in conjunction with `collect_layer()` for a lazy-ish process. -->
<!-- ```{r} -->
<!-- fl_query <- update_params( -->
<!--   county_fl,  -->
<!--   where = "STATE_NAME = 'New Hampshire'", -->
<!--   outFields = "STATE_NAME,POPULATION" -->
<!-- ) -->
<!-- fl_query -->
<!-- ``` -->
<!-- Notice the appended query. You can execute the query and bring the results into memory using `collect_layer()`. -->
<!-- ```{r} -->
<!-- collect_layer(fl_query) -->
<!-- ``` -->
<!-- ## Interfacing with `{arcgisbinding}` -->
<!-- - to do.... -->
<!-- ### dplyr-like syntax -->
<!-- You may not need all of these fields or features in your R session. If that's the case, it is more effective to bring in only a subset of the data that you need. We can limit what is brought into memory by using `select()` and `filter()` functions to build up a query that can be sent the rest API.  -->
<!-- The `select()` `FeatureLayer` method supports using `{tidyselect}` functions except the `where()` function. -->
<!-- ```{r} -->
<!-- county_fl |>  -->
<!--   select(contains("STATE")) -->
<!-- ``` -->
<!-- Now that we've ran `select()` on the `FeatureLayer` query parameters are printed out. The parameters will be sent along when the results are `collect()`ed.  -->
<!-- You can further subset your data by using `filter()`. Note that filtering is fairly limited ([see API documentation for more](https://developers.arcgis.com/rest/services-reference/enterprise/query-feature-service-layer-.htm#GUID-BC2AD141-3386-49FB-AA09-FF341145F614)). -->
<!-- ```{r} -->
<!-- county_query <- county_fl |>  -->
<!--   select(starts_with("STATE")) |>  -->
<!--   filter(STATE_NAME == "Alabama") -->
<!-- county_query -->
<!-- ``` -->
<!-- Bring it into memory with `collect()`. -->
<!-- ```{r} -->
<!-- res <- collect(county_query) -->
<!-- res -->
<!-- ``` -->
<!-- ### Other `FeatureLayer` functions -->
<!-- You can add any parameter that you'd like that is supported by the query parameters [listed in the documentation](https://developers.arcgis.com/rest/services-reference/enterprise/query-feature-service-layer-.htm#GUID-BC2AD141-3386-49FB-AA09-FF341145F614) with the using the `update_params()` function. -->
<!-- ```{r} -->
<!-- county_query |>  -->
<!--   update_params(returnIdsOnly = "true") |>  -->
<!--   collect() -->
<!-- ``` -->
<!-- Maybe, you want to clear our your query parameters, use `clear_query()`. -->
<!-- ```{r} -->
<!-- clear_query(county_query) -->
<!-- ``` -->
<!-- To preview only a few features from the feature layer use `head()` -->
<!-- ```{r} -->
<!-- head(county_query) -->
<!-- ``` -->
<!-- ## `FeatureServers` -->
<!-- FeatureServers contain one or more `FeatureLayers` or `Tables`.  -->
<!-- ```{r} -->
<!-- ft_srv <- feature_server( -->
<!--   "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/List_of_Providers/FeatureServer/" -->
<!--   ) -->
<!-- ft_srv -->
<!-- ``` -->
<!-- Access elements by ID with `get_layer()` -->
<!-- ```{r} -->
<!-- get_layer(ft_srv, 27) -->
<!-- ``` -->
<!-- Get a list of all elements. -->
<!-- ```{r} -->
<!-- get_all_layers(ft_srv) -->
<!-- ``` -->
<!-- ## Conversion to Esri JSON -->
<!-- sf objects can be converted to Esri JSON representation. These can be Geometry Objects, FeatureSets, or arrays of Features.  -->
<!-- `st_as_geometry()` works with objects of class `sfg` and creates a Geometry Object. `st_as_featureset()` creates a FeatureSet from an sf or sfc object. sfc objects will have 0 length attributes. `st_as_features()` works with an sf or sfc object.  -->
<!-- ```{r} -->
<!-- st_as_geometry(res[["geometry"]][[30]]) |>  -->
<!--   # print nicely with jsonify -->
<!--   jsonify::pretty_json() -->
<!-- ``` -->
<!-- ```{r} -->
<!-- st_as_featureset(res[30,]) |>  -->
<!--   jsonify::pretty_json() -->
<!-- ``` -->
