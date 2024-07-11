
<!-- README.md is generated from README.Rmd. Please edit that file -->

# arcgislayers <img src="man/figures/logo.svg" align="right" height="139" alt="" />

<!-- badges: start -->

[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/license/apache-2-0)
[![R-CMD-check](https://github.com/R-ArcGIS/arcgislayers/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/R-ArcGIS/arcgislayers/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![](https://cranlogs.r-pkg.org/badges/arcgislayers)](https://cran.r-project.org/package=arcgislayers)
<!-- badges: end -->

The goal of `{arcgislayers}` is to provide an R interface to the [ArcGIS
REST API](https://developers.arcgis.com/rest/).

## Installation

It is recommend you install and use the metapackage `{arcgis}`. You can
install the development version of arcgis like so:

``` r
remotes::install_github("r-arcgis/arcgis", dependencies = TRUE)
```

## Usage

### Creating a simple feature object from an ArcGIS FeatureLayer

``` r
library(arcgis)
#> Attaching core arcgis packages:
#> → arcgisutils v0.3.0
#> → arcgislayers v0.3.0
#> → arcgisgeocode v0.1.3
#> → arcgisplaces v0.1.0
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
#> Simple feature collection with 3143 features and 12 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -178.2176 ymin: 18.92179 xmax: -66.96927 ymax: 71.40624
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>    OBJECTID            NAME STATE_NAME STATE_FIPS  FIPS    SQMI
#> 1         1  Autauga County    Alabama         01 01001  604.37
#> 2         2  Baldwin County    Alabama         01 01003 1633.14
#> 3         3  Barbour County    Alabama         01 01005  904.52
#> 4         4     Bibb County    Alabama         01 01007  626.17
#> 5         5   Blount County    Alabama         01 01009  650.63
#> 6         6  Bullock County    Alabama         01 01011  625.14
#> 7         7   Butler County    Alabama         01 01013  777.88
#> 8         8  Calhoun County    Alabama         01 01015  612.27
#> 9         9 Chambers County    Alabama         01 01017  603.11
#> 10       10 Cherokee County    Alabama         01 01019  599.98
#>             POPULATION POP_SQMI STATE_ABBR COUNTY_FIPS Shape__Area
#> 1  1970-01-01 11:20:05     97.3         AL         001   0.1489034
#> 2  1970-01-03 11:22:47    141.9         AL         003   0.4044891
#> 3  1970-01-01 02:00:23     27.9         AL         005   0.2224307
#> 4  1970-01-01 01:11:33     35.6         AL         007   0.1577359
#> 5  1970-01-01 11:25:34     90.9         AL         009   0.1675296
#> 6  1969-12-31 21:52:37     16.6         AL         011   0.1557273
#> 7  1970-01-01 00:17:31     24.5         AL         013   0.1927305
#> 8  1970-01-02 03:20:41    190.2         AL         015   0.1523369
#> 9  1970-01-01 04:39:32     57.7         AL         017   0.1531136
#> 10 1970-01-01 01:56:11     41.6         AL         019   0.1527217
#>    Shape__Length                       geometry
#> 1       1.884137 POLYGON ((-86.82067 32.3473...
#> 2       3.678276 POLYGON ((-87.97309 31.1648...
#> 3       2.218514 POLYGON ((-85.74337 31.6262...
#> 4       1.852453 POLYGON ((-87.41986 33.0117...
#> 5       2.067456 POLYGON ((-86.96799 33.8604...
#> 6       2.006250 POLYGON ((-85.4114 32.15551...
#> 7       1.769462 POLYGON ((-86.44912 31.9712...
#> 8       2.149825 POLYGON ((-85.79353 33.5634...
#> 9       1.637226 POLYGON ((-85.58963 32.7313...
#> 10      1.794142 POLYGON ((-85.41657 34.0869...
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
#>    STATE_ABBR          POPULATION                       geometry
#> 1          AZ 1970-02-20 22:56:08 POLYGON ((-111.0425 33.4759...
#> 2          AZ 1970-01-12 20:50:33 POLYGON ((-110.4522 31.7360...
#> 3          CA 1970-01-20 06:19:13 POLYGON ((-121.4721 37.4777...
#> 4          CA 1970-01-14 06:52:07 POLYGON ((-122.3076 37.8917...
#> 5          CA 1970-01-12 11:10:54 POLYGON ((-120.6636 36.2787...
#> 6          CA 1970-04-26 17:40:09 POLYGON ((-118.1067 33.7475...
#> 7          CA 1970-02-06 16:16:29 POLYGON ((-117.509 33.50848...
#> 8          CA 1970-01-28 18:43:05 POLYGON ((-116.0824 33.4258...
#> 9          CA 1970-01-19 03:17:35 POLYGON ((-121.6652 38.1692...
#> 10         CA 1970-01-26 01:00:54 POLYGON ((-117.7832 33.9507...
```

For `FeatureLayer` and `Table` objects, and sometimes `ImageServer`s,
the `list_fields()` function can be helpful to check available
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
#>   `/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/sf/shape/nc.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
```

``` r

arc_select(
  county_fl,
  filter_geom = sf::st_bbox(nc[1,])
)
#> Simple feature collection with 6 features and 12 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -82.0477 ymin: 35.98946 xmax: -80.83795 ymax: 36.80746
#> Geodetic CRS:  WGS 84
#>   OBJECTID             NAME     STATE_NAME STATE_FIPS  FIPS   SQMI
#> 1     1890 Alleghany County North Carolina         37 37005 236.26
#> 2     1892      Ashe County North Carolina         37 37009 429.38
#> 3     1982   Watauga County North Carolina         37 37189 313.32
#> 4     1984    Wilkes County North Carolina         37 37193 756.33
#> 5     2471   Johnson County      Tennessee         47 47091 302.69
#> 6     2855   Grayson County       Virginia         51 51077 445.57
#>            POPULATION POP_SQMI STATE_ABBR COUNTY_FIPS Shape__Area Shape__Length
#> 1 1969-12-31 22:01:28     46.1         NC         005  0.06140165      1.231232
#> 2 1970-01-01 02:22:57     61.9         NC         009  0.11428581      1.442112
#> 3 1970-01-01 10:01:26    172.6         NC         189  0.08142272      1.287674
#> 4 1970-01-01 13:19:29     87.2         NC         193  0.19911944      1.984232
#> 5 1969-12-31 23:59:08     59.3         TN         091  0.07960385      1.290607
#> 6 1969-12-31 23:15:33     34.4         VA         077  0.11578917      1.945424
#>                         geometry
#> 1 POLYGON ((-81.2397 36.36549...
#> 2 POLYGON ((-81.47258 36.2344...
#> 3 POLYGON ((-81.80605 36.1046...
#> 4 POLYGON ((-81.02037 36.0350...
#> 5 POLYGON ((-81.74091 36.3919...
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
article](https://r.esri.com/r-bridge-site/location-services/connecting-to-a-portal.html).

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
website](https://r.esri.com/r-bridge-site/location-services/workflows/add-delete-update.html).

These functions all require authorization since data cannot be published
or modified anonymously in ArcGIS Online and ArcGIS Enterprise.
