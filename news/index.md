# Changelog

## arcgislayers (development version)

### New features

### Bug Fixes

- [`arc_select()`](https://developers.arcgis.com/r-bridge/reference/arc_select.md)
  returns an empty `data.frame` instead of `NULL` when no features are
  returned from a query

### Breaking changes

## arcgislayers 0.5.1

CRAN release: 2025-10-22

### New features

- `encode_field_values(codes = "replace-valid")` allows users to retain
  invalid values when replacing coded values.
  ([\#267](https://github.com/R-ArcGIS/arcgislayers/issues/267))

## arcgislayers 0.5.0

CRAN release: 2025-09-19

### New features

- New
  [`update_attachments()`](https://developers.arcgis.com/r-bridge/reference/update_attachments.md)
  function modifies a feature service’s attachments
  <https://github.com/R-ArcGIS/arcgislayers/pull/277>
- [`arc_open()`](https://developers.arcgis.com/r-bridge/reference/arc_open.md)
  now works with item IDs or a variety of URLs such as item, user,
  group, and more <https://github.com/R-ArcGIS/arcgislayers/pull/275>
- `add_definition()`
  ([\#178](https://github.com/R-ArcGIS/arcgislayers/issues/178)),
  `update_definition()`
  ([\#127](https://github.com/R-ArcGIS/arcgislayers/issues/127)), and
  `delete_definition()` functions for FeatureServer and FeatureLayer
  objects.

### Bug Fixes

- Improve
  [`update_features()`](https://developers.arcgis.com/r-bridge/reference/modify.md)
  with an error message when the `objectid` is not an `integer`
  ([\#250](https://github.com/R-ArcGIS/arcgislayers/issues/250))
- [`get_layer()`](https://developers.arcgis.com/r-bridge/reference/get_layer.md)
  warns as expected on invalid layer names values.
  ([\#251](https://github.com/R-ArcGIS/arcgislayers/issues/251))
- [`encode_field_values()`](https://developers.arcgis.com/r-bridge/reference/encode_field_values.md)
  handles numeric columns with coded value domains without warnings or
  errors. ([\#237](https://github.com/R-ArcGIS/arcgislayers/issues/237))
- [`encode_field_values()`](https://developers.arcgis.com/r-bridge/reference/encode_field_values.md)
  now properly skips `range` field types
  ([\#263](https://github.com/R-ArcGIS/arcgislayers/issues/263))

### Breaking changes

- [`update_features()`](https://developers.arcgis.com/r-bridge/reference/modify.md)
  is now parallelized and sends updates in chunks see `chunk_size`
  argument and `progress` arguments. The return type is now a
  `data.frame` and not a list with `updateResults`
- [`delete_features()`](https://developers.arcgis.com/r-bridge/reference/modify.md)
  is now parallelized and deletes in chunks. See above.
- [arcgislayers](https://developers.arcgis.com/r-bridge) now depends on
  R 4.2 or higher.
- [`list_service_raster_fns()`](https://developers.arcgis.com/r-bridge/reference/raster_fns.md)
  is now deprecated in favor of
  [`list_raster_fns()`](https://developers.arcgis.com/r-bridge/reference/raster_fns.md)

## arcgislayers 0.4.0

CRAN release: 2025-04-15

### New features

- Address bug in JSON parsing by bumping the version of RcppSimdJson
- Improve handling of `filter_geom` by
  [`arc_select()`](https://developers.arcgis.com/r-bridge/reference/arc_select.md)
  by warning if applying
  [`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html)
  to the filter does not generate a length 1 sfc, or if `filter_geom` is
  supplied when accessing a Table, or if `filter_geom` is empty
  ([@elipousson](https://github.com/elipousson),
  [\#166](https://github.com/R-ArcGIS/arcgislayers/issues/166))
- Export
  [`set_layer_aliases()`](https://developers.arcgis.com/r-bridge/reference/set_layer_aliases.md)
  (previously used internally by
  [`arc_read()`](https://developers.arcgis.com/r-bridge/reference/arc_read.md))
  to allow use of alias values with data returned by
  [`arc_select()`](https://developers.arcgis.com/r-bridge/reference/arc_select.md)
  ([\#169](https://github.com/R-ArcGIS/arcgislayers/issues/169)).
- Add new
  [`encode_field_values()`](https://developers.arcgis.com/r-bridge/reference/encode_field_values.md)
  function to support replacement or labeling of values with coded value
  domains
  ([\#134](https://github.com/R-ArcGIS/arcgislayers/issues/134)).
- Improve input checks for
  [`get_layer()`](https://developers.arcgis.com/r-bridge/reference/get_layer.md),
  [`get_all_layers()`](https://developers.arcgis.com/r-bridge/reference/get_layer.md),
  and
  [`get_layers()`](https://developers.arcgis.com/r-bridge/reference/get_layer.md)
  to require FeatureServer, MapServer, or GroupLayer input objects.

### Bug fixes

- Adjusts
  [`arc_select()`](https://developers.arcgis.com/r-bridge/reference/arc_select.md)
  to not error out when `query` capability isn’t explicitly listed.
  Instead `cli_alert_danger()` is used to communicate the issue
  <https://github.com/R-ArcGIS/arcgislayers/pull/230>
- [`arc_select()`](https://developers.arcgis.com/r-bridge/reference/arc_select.md)
  includes argument name in error message when `...` contains non-string
  values. <https://github.com/R-ArcGIS/arcgislayers/issues/226>

### Breaking changes

- `dplyr` methods for `collect()`, `select()`, and
  [`filter()`](https://rdrr.io/r/stats/filter.html) have been removed.
  <https://github.com/R-ArcGIS/arcgislayers/issues/111>
  <https://github.com/R-ArcGIS/arcgislayers/issues/224>
  <https://github.com/R-ArcGIS/arcgislayers/issues/218>
- Soft deprecate `arc_read(col_names = "alias")` (use
  `arc_read(alias = "replace")` instead)

## arcgislayers 0.3.1

CRAN release: 2024-09-27

### Bug fixes

- `page_size` resulted in error due to introduction of type-check. Fixed
  and added test to avoid in the future.
  [\#205](https://github.com/R-ArcGIS/arcgislayers/issues/205)
- Add warning if
  [`arc_select()`](https://developers.arcgis.com/r-bridge/reference/arc_select.md)
  results include fewer features than expected from request
  [\#220](https://github.com/R-ArcGIS/arcgislayers/issues/220)

### New features

- [`arc_raster()`](https://developers.arcgis.com/r-bridge/reference/arc_raster.md)
  gains an argument `raster_fn` which takes a character scalar and
  performs a raster function server side before returning results
- [`list_service_raster_fns()`](https://developers.arcgis.com/r-bridge/reference/raster_fns.md)
  is a new helper function to list available raster functions for an
  `ImageServer`
- [`arc_open()`](https://developers.arcgis.com/r-bridge/reference/arc_open.md)
  ignores queries included in input URLs and retains any custom queries
  in the `query` attribute for `Table` and `FeatureLayer`s.
  ([\#215](https://github.com/R-ArcGIS/arcgislayers/issues/215))

### Breaking changes

## arcgislayers 0.3.0

CRAN release: 2024-07-05

- [`arc_open()`](https://developers.arcgis.com/r-bridge/reference/arc_open.md)
  will now work on any resource that works when `f=json` is set in the
  query parameters closes
  [\#163](https://github.com/R-ArcGIS/arcgislayers/issues/163)
- Now uses [`{arcpbf}`](https://r.esri.com/arcpbf/index.html) when a
  layer supports protocol buffers.
  - This is an ~3x speed improvement over json processing.
- New
  [`query_layer_attachments()`](https://developers.arcgis.com/r-bridge/reference/attachments.md)
  and
  [`download_attachments()`](https://developers.arcgis.com/r-bridge/reference/attachments.md)
  help you access and download attachments to a layer
- [`arc_raster()`](https://developers.arcgis.com/r-bridge/reference/arc_raster.md)
  now downloads the exported image to a temp file instead of creating a
  connection to the url returned. This fixes an issue where rasters
  would stop working after the url had been removed.
- Add `alias` argument to
  [`arc_read()`](https://developers.arcgis.com/r-bridge/reference/arc_read.md)
  allowing replacement or labelling of field names with alias values
  ([\#169](https://github.com/R-ArcGIS/arcgislayers/issues/169))
- Add
  [`pull_field_aliases()`](https://developers.arcgis.com/r-bridge/reference/utils.md)
  utility function
- [`arc_select()`](https://developers.arcgis.com/r-bridge/reference/arc_select.md)
  now uses
  [`arcgisutils::rbind_results()`](https://rdrr.io/pkg/arcgisutils/man/rbind_results.html)
  for faster row-binding if [collapse](https://fastverse.org/collapse/),
  [data.table](https://r-datatable.com),
  [vctrs](https://vctrs.r-lib.org/) are installed
  ([\#175](https://github.com/R-ArcGIS/arcgislayers/issues/175))
- Preserve order of `fields` column names for
  [`arc_select()`](https://developers.arcgis.com/r-bridge/reference/arc_select.md)
  (fixes minor bug with `arc_read` handling of `col_names`)
  ([\#185](https://github.com/R-ArcGIS/arcgislayers/issues/185))
- Set CRS for a FeatureLayer or ImageServer using `"wkid"` or `"wkt"`
  value if `"latestWkid"` is missing.
  ([\#188](https://github.com/R-ArcGIS/arcgislayers/issues/188))
- Fix issue with
  [`arc_select()`](https://developers.arcgis.com/r-bridge/reference/arc_select.md)
  when layer can’t support pagination.
  ([\#191](https://github.com/R-ArcGIS/arcgislayers/issues/191))

## arcgislayers 0.2.0

CRAN release: 2024-02-27

- initial CRAN release

## arcgislayers 0.1.0

- [`arc_open()`](https://developers.arcgis.com/r-bridge/reference/arc_open.md)
  no longer removes `NULL` properties h/t
  [@elipousson](https://github.com/elipousson)

- includes `page_size` argument to
  [`arc_select()`](https://developers.arcgis.com/r-bridge/reference/arc_select.md)
  allowing users to return smaller page sizes and avoid timeouts for
  dense geometries

- Add support for `GroupLayer`s

- Add
  [`arc_read()`](https://developers.arcgis.com/r-bridge/reference/arc_read.md)
  with support for `name_repair` argument using
  [vctrs](https://vctrs.r-lib.org/)
  ([\#108](https://github.com/R-ArcGIS/arcgislayers/issues/108))

- Add
  [`get_layer_estimates()`](https://developers.arcgis.com/r-bridge/reference/get_layer_estimates.md)
  to retrieve estimate info such as the number of features and the
  extent of the layer

- Add
  [`truncate_layer()`](https://developers.arcgis.com/r-bridge/reference/truncate_layer.md)
  to support truncate and append workflow

- Add support for opening `MapServers`
  <https://github.com/R-ArcGIS/arcgislayers/pull/83>

- [`arc_open()`](https://developers.arcgis.com/r-bridge/reference/arc_open.md)
  with a layer that does not support `Query` sets the `n` attribute
  to`NA` <https://github.com/R-ArcGIS/arcgislayers/pull/83>

  - Print method will show something like
    `<FeatureLayer <NA features, 10 fields>>`

- [`arc_select()`](https://developers.arcgis.com/r-bridge/reference/arc_select.md)
  is now supported for `ImageServer`s
  [\#78](https://github.com/R-ArcGIS/arcgislayers/issues/78)

- [`add_features()`](https://developers.arcgis.com/r-bridge/reference/modify.md)
  is now paginated and performed in parallel. It gains an argument
  `chunk_size` which determines the maximum number of rows to be added
  to a feature service.

- adds
  [`get_layers()`](https://developers.arcgis.com/r-bridge/reference/get_layer.md)
  which can fetch multiple items from a `FeatureServer` or `MapServer`

  - new utility function
    [`list_items()`](https://developers.arcgis.com/r-bridge/reference/utils.md)

- adds cli as an explicit import (has been implicitly imported by httr2)

- repository made public

- add lifecycle badges to all exported functions
  <https://github.com/R-ArcGIS/arcgislayers/pull/101>

- **Breaking**:

  - `token` arguments are required to be a valid `httr2_token` object
    (strings are not supported).
  - all `host` arguments are removed. Instead, the host is fetched from
    the `token`.
  - all `user` arguments are removed. Instead, the username is fetched
    from the `token`. If it is not found, an error is thrown.
