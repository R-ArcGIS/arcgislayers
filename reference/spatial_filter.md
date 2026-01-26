# Prepare JSON for use as a spatial filter based on feature geometry or bounding box input

`prepare_spatial_filter()` prepares a named list with ESRI JSON geometry
for use as a spatial filter based on a a `sfc`, `sfg`, or `bbox` input
object.

`match_spatial_rel()` takes a scalar character vector with a predicate
name to a type of ESRI spatial relation.

## Usage

``` r
prepare_spatial_filter(
  filter_geom,
  crs,
  predicate,
  error_call = rlang::caller_env()
)

match_spatial_rel(predicate, error_call = rlang::caller_env())
```

## Arguments

- filter_geom:

  an object of class `bbox`, `sfc` or `sfg` used to filter query results
  based on a predicate function.

- crs:

  a representation of a coordinate reference system.

- predicate:

  Spatial predicate to use with `filter_geom`. Default `"intersects"`.
  Possible options are `"intersects"`, `"contains"`, `"crosses"`,
  `"overlaps"`, `"touches"`, and `"within"`.

- error_call:

  default
  [`rlang::caller_env()`](https://rlang.r-lib.org/reference/stack.html).

## Value

`prepare_spatial_filter()` returns a named list with the `geometryType`,
`geometry` (as Esri JSON), and spatial relation predicate.

`match_spatial_rel()` returns one of the following spatial binary
predicates:

- esriSpatialRelIntersects

- esriSpatialRelContains

- esriSpatialRelCrosses

- esriSpatialRelOverlaps

- esriSpatialRelTouches

- esriSpatialRelWithin

## Details

Using `sfc` objects as `filter_geom`

**\[experimental\]**

If an `sfc` object is provided it will be transformed to the layers
spatial reference. If the `sfc` is missing a CRS (or is an `sfg` object)
it is assumed to use the same spatial reference as the FeatureLayer. If
the `sfc` object has multiple features, the features are unioned with
[`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html).
If an `sfc` object has `MULTIPOLYGON` geometry, the features are cast to
`POLYGON` geometry and only the first element is used.

## Examples

``` r
prepare_spatial_filter(sf::st_point(c(0, 5)), 4326, "intersects")
#> $geometryType
#> [1] "esriGeometryPoint"
#> 
#> $geometry
#> [1] "{\"x\":0.0,\"y\":5.0,\"spatialReference\":{\"wkid\":4326}}"
#> 
#> $spatialRel
#> [1] "esriSpatialRelIntersects"
#> 
```
