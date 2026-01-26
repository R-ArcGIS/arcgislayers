# Add Features to Feature Layer

Delete features from a feature layer based on object ID, a where clause,
or a spatial filter.

## Usage

``` r
add_features(
  x,
  .data,
  chunk_size = 500,
  match_on = c("name", "alias"),
  rollback_on_failure = TRUE,
  progress = TRUE,
  token = arc_token()
)

delete_features(
  x,
  object_ids = NULL,
  where = NULL,
  filter_geom = NULL,
  predicate = "intersects",
  rollback_on_failure = TRUE,
  chunk_size = 500,
  progress = TRUE,
  token = arc_token()
)

update_features(
  x,
  .data,
  chunk_size = 500,
  match_on = c("name", "alias"),
  rollback_on_failure = TRUE,
  progress = TRUE,
  token = arc_token()
)
```

## Arguments

- x:

  an object of class `FeatureLayer`

- .data:

  an object of class `sf` or `data.frame`

- chunk_size:

  the maximum number of features to add at a time

- match_on:

  whether to match on the alias or the field name. Default, the alias.
  See Details for more.

- rollback_on_failure:

  default `TRUE`. Specifies whether the edits should be applied only if
  all submitted edits succeed.

- progress:

  default `TRUE`. A progress bar to be rendered by `httr2` to track
  requests.

- token:

  default `arc_token()`. An `httr2_token`.

- object_ids:

  a numeric vector of object IDs to be deleted.

- where:

  a simple SQL where statement indicating which features should be
  deleted. When the where statement evaluates to `TRUE`, those values
  will be deleted.

- filter_geom:

  an object of class `bbox`, `sfc` or `sfg` used to filter query results
  based on a predicate function.

- predicate:

  Spatial predicate to use with `filter_geom`. Default `"intersects"`.
  Possible options are `"intersects"`, `"contains"`, `"crosses"`,
  `"overlaps"`, `"touches"`, and `"within"`.

## Value

- `add_features()` returns a `data.frame` with columns `objectId`,
  `uniqueId`, `globalId`, `success`

- `update_features()` returns a list with an element named
  `updateResults` which is a `data.frame` with columns `objectId`,
  `uniqueId`, `globalId`, `success`

- `delete_features()` returns a list with an element named
  `deleteResults` which is a `data.frame` with columns `objectId`,
  `uniqueId`, `globalId`, `success`

## Details

**\[experimental\]**

For a more detailed guide to adding, updating, and deleting features,
view the tutorial on the [R-ArcGIS Bridge
website](https://developers.arcgis.com/r-bridge/editing/overview/).

Regarding the `match_on` argument:when publishing an object to an ArcGIS
Portal from R, the object's names are provided as the alias. The
object's names are subject to change according to the standards of the
ArcGIS REST API. For example. `"Sepal.Length"` is changed to
`"Sepal_Width"` in the `name` field but the alias remains
`"Sepal.Length"`. For that reason, we match on the alias name by
default. Change this argument to match based on the field name.

## Examples

``` r
if (FALSE) { # \dontrun{
  # this is pseudo-code and will not work
  flayer <- arc_open(furl)

  # add sf objects to existing feature service
  add_features(flayer, sfobj)

  # delete all features
  delete_features(flayer, where = "1 = 1")

  # update features
  update_features(flayer, dfobj)
} # }
```
