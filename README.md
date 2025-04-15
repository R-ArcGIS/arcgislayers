
<!-- README.md is generated from README.Rmd. Please edit that file -->

# arcgislayers <img src="man/figures/logo.svg" align="right" height="139" alt="" />

<!-- badges: start -->

[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/license/apache-2-0)
[![R-CMD-check](https://github.com/R-ArcGIS/arcgislayers/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/R-ArcGIS/arcgislayers/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![](https://cranlogs.r-pkg.org/badges/arcgislayers)](https://cran.r-project.org/package=arcgislayers)
<!-- badges: end -->

`{arcgislayers}` simplifies accessing and managing data the ArcGIS
Ecosystem. With it you can:

- Read data from ArcGIS Online, Enterprise, Survey123, Location
  Platform, Hub, and more
- Read Imagery as `SpatRaster` from `{terra}`
- Read Feature Services as `sf` objects
- Publish {sf} objects and data.frame’s as Feature Services
- Query and download attachments from Survey123

## Installation

It is recommend you install and use the metapackage `{arcgis}`. You can
install the development version of arcgis like so:

``` r
install.packages(
  "arcgis",
  repos = c("https://r-arcgis.r-universe.dev", "https://cloud.r-project.org")
)
```

## Usage

### Read data from a Feature Service

``` r
library(arcgis)
#> Attaching core arcgis packages:
#> → arcgisutils v0.3.3
#> → arcgislayers v0.3.1.9000
#> → arcgisgeocode v0.2.2
#> → arcgisplaces v0.1.1
```

`arc_open()` takes a URL to create a reference to a remote ArcGIS layer,
server, or table. The function can return any of the following classes
(corresponding to different ArcGIS service types):

- `FeatureLayer`
- `Table`
- `FeatureServer`
- `ImageServer`
- `MapServer`
- `GroupLayer`

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

You can then use `arc_select()` to query the feature layer object and
return an `sf` object.

If no arguments are provided to `arc_select()` the entire feature layer
is returned in memory as an `sf` object.

``` r
arc_select(county_fl)
#> Simple feature collection with 3144 features and 12 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -178.2176 ymin: 18.92179 xmax: -66.96927 ymax: 71.40624
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>    OBJECTID               NAME   STATE_NAME STATE_FIPS  FIPS SQMI POPULATION
#> 1         1 Grand Forks County North Dakota         38 38035 51.3      73170
#> 2         2       Grant County North Dakota         38 38037  1.4       2301
#> 3         3      Griggs County North Dakota         38 38039  3.1       2306
#> 4         4   Hettinger County North Dakota         38 38041  2.1       2489
#> 5         5      Kidder County North Dakota         38 38043  1.6       2394
#> 6         6     LaMoure County North Dakota         38 38045  3.5       4093
#> 7         7       Logan County North Dakota         38 38047  1.8       1876
#> 8         8     McHenry County North Dakota         38 38049  2.8       5345
#> 9         9    McIntosh County North Dakota         38 38051  2.5       2530
#> 10       10    McKenzie County North Dakota         38 38053  5.3      14704
#>    POP_SQMI STATE_ABBR COUNTY_FIPS Shape__Area Shape__Length
#> 1      50.8         ND         035   0.4503702      2.961625
#> 2       1.4         ND         037   0.5040677      3.413506
#> 3       3.2         ND         039   0.2230339      1.949037
#> 4       2.2         ND         041   0.3427475      2.691898
#> 5       1.7         ND         043   0.4378065      2.719487
#> 6       3.6         ND         045   0.3502662      2.702124
#> 7       1.9         ND         047   0.3090110      2.454735
#> 8       2.8         ND         049   0.5877751      3.261400
#> 9       2.5         ND         051   0.2971127      2.421863
#> 10      5.1         ND         053   0.8862776      4.625264
#>                          geometry
#> 1  POLYGON ((-96.88943 47.6739...
#> 2  POLYGON ((-102.0034 46.0528...
#> 3  POLYGON ((-97.96167 47.2449...
#> 4  POLYGON ((-102.0034 46.2058...
#> 5  POLYGON ((-100.0885 46.6357...
#> 6  POLYGON ((-99.00921 46.2830...
#> 7  POLYGON ((-99.04408 46.2833...
#> 8  POLYGON ((-100.9647 47.8540...
#> 9  POLYGON ((-99.87578 45.9435...
#> 10 POLYGON ((-103.6744 47.3320...
```

### Filtering using `where` or `filter_geom` arguments

You can also use the `fields` argument to select columns or the `where`
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
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -158.2674 ymin: 21.24986 xmax: -71.02671 ymax: 47.77552
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>    STATE_ABBR POPULATION                       geometry
#> 1          OH    1264817 POLYGON ((-81.37707 41.3463...
#> 2          OH    1323807 POLYGON ((-83.24282 39.8044...
#> 3          PA    1250578 POLYGON ((-79.86399 40.2007...
#> 4          PA    1603797 POLYGON ((-75.1429 39.8816,...
#> 5          HI    1016508 POLYGON ((-157.6733 21.2980...
#> 6          IL    5275541 POLYGON ((-88.26711 41.9887...
#> 7          AZ    4420568 POLYGON ((-111.0425 33.4759...
#> 8          AZ    1043433 POLYGON ((-110.4522 31.7360...
#> 9          CA    1682353 POLYGON ((-121.4721 37.4777...
#> 10         CA    1165927 POLYGON ((-122.3076 37.8917...
```

For `FeatureLayer` and `Table` objects, and sometimes `ImageServer`s,
the `list_fields()` function can be helpful to check available
attributes and build a `where` query:

``` r
list_fields(county_fl)
#>             name                 type                  alias       sqlType nullable
#> 1       OBJECTID     esriFieldTypeOID               OBJECTID  sqlTypeOther    FALSE
#> 2           NAME  esriFieldTypeString                   Name  sqlTypeOther     TRUE
#> 3     STATE_NAME  esriFieldTypeString             State Name  sqlTypeOther     TRUE
#> 4     STATE_FIPS  esriFieldTypeString             State FIPS  sqlTypeOther     TRUE
#> 5           FIPS  esriFieldTypeString                   FIPS  sqlTypeOther     TRUE
#> 6           SQMI  esriFieldTypeDouble   Area in square miles  sqlTypeOther     TRUE
#> 7     POPULATION esriFieldTypeInteger  2020 Total Population  sqlTypeOther     TRUE
#> 8       POP_SQMI  esriFieldTypeDouble People per square mile  sqlTypeOther     TRUE
#> 9     STATE_ABBR  esriFieldTypeString     State Abbreviation  sqlTypeOther     TRUE
#> 10   COUNTY_FIPS  esriFieldTypeString            County FIPS  sqlTypeOther     TRUE
#> 11   Shape__Area  esriFieldTypeDouble            Shape__Area sqlTypeDouble     TRUE
#> 12 Shape__Length  esriFieldTypeDouble          Shape__Length sqlTypeDouble     TRUE
#>    editable domain defaultValue length
#> 1     FALSE     NA           NA     NA
#> 2      TRUE     NA           NA     50
#> 3      TRUE     NA           NA     20
#> 4      TRUE     NA           NA      2
#> 5      TRUE     NA           NA      5
#> 6      TRUE     NA           NA     NA
#> 7      TRUE     NA           NA     NA
#> 8      TRUE     NA           NA     NA
#> 9      TRUE     NA           NA      2
#> 10     TRUE     NA           NA      3
#> 11    FALSE     NA           NA     NA
#> 12    FALSE     NA           NA     NA
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
#>   `/Users/josiahparry/Library/R/arm64/4.5/library/sf/shape/nc.shp' 
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
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -82.0477 ymin: 35.98946 xmax: -80.83795 ymax: 36.80746
#> Geodetic CRS:  WGS 84
#>   OBJECTID             NAME     STATE_NAME STATE_FIPS  FIPS  SQMI POPULATION
#> 1      467   Johnson County      Tennessee         47 47091  58.8      17948
#> 2     1924 Alleghany County North Carolina         37 37005  47.0      10888
#> 3     1926      Ashe County North Carolina         37 37009  60.3      26577
#> 4     2016   Watauga County North Carolina         37 37189 174.4      54086
#> 5     2018    Wilkes County North Carolina         37 37193  84.9      65969
#> 6     2995   Grayson County       Virginia         51 51077  34.1      15333
#>   POP_SQMI STATE_ABBR COUNTY_FIPS Shape__Area Shape__Length
#> 1     59.3         TN         091  0.07960385      1.290607
#> 2     46.1         NC         005  0.06140165      1.231232
#> 3     61.9         NC         009  0.11428581      1.442112
#> 4    172.6         NC         189  0.08142272      1.287674
#> 5     87.2         NC         193  0.19911944      1.984232
#> 6     34.4         VA         077  0.11578917      1.945424
#>                         geometry
#> 1 POLYGON ((-81.74091 36.3919...
#> 2 POLYGON ((-81.2397 36.36549...
#> 3 POLYGON ((-81.47258 36.2344...
#> 4 POLYGON ((-81.80605 36.1046...
#> 5 POLYGON ((-81.02037 36.0350...
#> 6 POLYGON ((-81.34512 36.5729...
```

### Creating a `SpatRaster` from an ArcGIS ImageServer

A `SpatRaster` object from the `{terra}` package can be extracted from
an `ImageServer` using `arc_raster()`.

`arc_raster()` will extract the area defined by `xmin`, `ymin`, `xmax`,
and `ymax`. You can optionally specify the `width` and `height` of the
resultant image. Use `format` to define what type of image is returned.

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

## Authorization and publication

Authorization is not required for reading any public data sources.

Workflows that require authorization include:

- interacting with
  [non-public](https://doc.arcgis.com/en/arcgis-online/share-maps/share-items.htm)
  services,
- publishing a new service (the authorized user must also have
  [publishing
  privileges](https://doc.arcgis.com/en/arcgis-online/administer/roles.htm)),
  and
- modifying or deleting any existing service (the authorized user must
  also have [edit
  access](https://doc.arcgis.com/en/arcgis-online/manage-data/manage-editing-hfl.htm)
  to the service).

### Accessing non-public data

The same functions for reading public ArcGIS Online and Enterprise
services (such as
`arc_open()`,`arc_read()`,`arc_select()`,`arc_raster()`, etc.) can be
used to read data from non-public services by using the `token`
argument. For more information on tokens and authorization functions,
see the [authorization
article](https://developers.arcgis.com/r-bridge/authentication/connecting-to-a-portal/).

### Publishing and modifying services from R

The package includes functions to publish data to an ArcGIS Portal:

- `add_item()`: Creates a new FeatureCollection from a `sf` or
  `data.frame` object
- `publish_item()`: Publishes an existing FeatureLayer
- `publish_layer()`: is a higher level wrapper around both `add_item()`
  and `publish_item()`

There are also functions to add or modify data including
`add_features()`, `update_features()`, and `delete_features()`. For a
more detailed guide to adding, updating, and deleting features, view the
tutorial on the [R-ArcGIS Bridge
website](https://developers.arcgis.com/r-bridge).

These functions all require authorization since data cannot be published
or modified anonymously in ArcGIS Online and ArcGIS Enterprise.
