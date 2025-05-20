# arcgislayers development

## New features

- Add `update_definition()` function (#127) for FeatureServer and FeatureLayer objects.

# arcgislayers 0.4.0


## New features

- Address bug in JSON parsing by bumping the version of RcppSimdJson
- Improve handling of `filter_geom` by `arc_select()` by warning if applying `sf::st_union()` to the filter does not generate a length 1 sfc, or if `filter_geom` is supplied when accessing a Table, or if `filter_geom` is empty (@elipousson, #166)
- Export `set_layer_aliases()` (previously used internally by `arc_read()`) to allow use of alias values with data returned by `arc_select()` (#169).
- Add new `encode_field_values()` function to support replacement or labeling of values with coded value domains (#134).
- Improve input checks for `get_layer()`, `get_all_layers()`, and `get_layers()` to require FeatureServer, MapServer, or GroupLayer input objects.

## Bug fixes

- Adjusts `arc_select()` to not error out when `query` capability isn't explicitly listed. Instead `cli_alert_danger()` is used to communicate the issue <https://github.com/R-ArcGIS/arcgislayers/pull/230>
- `arc_select()` includes argument name in error message when `...` contains non-string values. <https://github.com/R-ArcGIS/arcgislayers/issues/226>

## Breaking changes

- `dplyr` methods for `collect()`, `select()`, and `filter()` have been removed. <https://github.com/R-ArcGIS/arcgislayers/issues/111> <https://github.com/R-ArcGIS/arcgislayers/issues/224> <https://github.com/R-ArcGIS/arcgislayers/issues/218>
- Soft deprecate `arc_read(col_names = "alias")` (use `arc_read(alias = "replace")` instead)

# arcgislayers 0.3.1

## Bug fixes

- `page_size` resulted in error due to introduction of type-check. Fixed and added test to avoid in the future.  [#205](https://github.com/R-ArcGIS/arcgislayers/issues/205)
- Add warning if `arc_select()` results include fewer features than expected from request [#220](https://github.com/R-ArcGIS/arcgislayers/issues/220)

## New features

- `arc_raster()` gains an argument `raster_fn` which takes a character scalar and performs a raster function server side before returning results
- `list_service_raster_fns()` is a new helper function to list available raster functions for an `ImageServer`
- `arc_open()` ignores queries included in input URLs and retains any custom queries in the `query` attribute for `Table` and `FeatureLayer`s. ([#215](https://github.com/R-ArcGIS/arcgislayers/issues/215))

## Breaking changes 

# arcgislayers 0.3.0

- `arc_open()` will now work on any resource that works when `f=json` is set in the query parameters closes [#163](https://github.com/R-ArcGIS/arcgislayers/issues/163)
- Now uses [`{arcpbf}`](https://r.esri.com/arcpbf/index.html) when a layer supports protocol buffers. 
  - This is an ~3x speed improvement over json processing.
- New `query_layer_attachments()` and `download_attachments()` help you access and download attachments to a layer
- `arc_raster()` now downloads the exported image to a temp file instead of creating a connection to the url returned. This fixes an issue where rasters would stop working after the url had been removed. 
- Add `alias` argument to `arc_read()` allowing replacement or labelling of field names with alias values (#169)
- Add `pull_field_aliases()` utility function
- `arc_select()` now uses `arcgisutils::rbind_results()` for faster row-binding if `{collapse}`, `{data.table}`, `{vctrs}` are installed (#175)
- Preserve order of `fields` column names for `arc_select()` (fixes minor bug with `arc_read` handling of `col_names`) (#185)
- Set CRS for a FeatureLayer or ImageServer using `"wkid"` or `"wkt"` value if `"latestWkid"` is missing. (#188)
- Fix issue with `arc_select()` when layer can't support pagination. (#191)

# arcgislayers 0.2.0

- initial CRAN release

# arcgislayers 0.1.0 

- `arc_open()` no longer removes `NULL` properties h/t [@elipousson](https://github.com/elipousson)
- includes `page_size` argument to `arc_select()` allowing users to return smaller page sizes and avoid timeouts for dense geometries
- Add support for `GroupLayer`s
- Add `arc_read()` with support for `name_repair` argument using `{vctrs}` (#108)
- Add `get_layer_estimates()` to retrieve estimate info such as the number of features and the extent of the layer
- Add `truncate_layer()` to support truncate and append workflow
- Add support for opening `MapServers` <https://github.com/R-ArcGIS/arcgislayers/pull/83>
- `arc_open()` with a layer that does not support `Query` sets the `n` attribute to`NA` <https://github.com/R-ArcGIS/arcgislayers/pull/83>
  - Print method will show something like `<FeatureLayer <NA features, 10 fields>>`
- `arc_select()` is now supported for `ImageServer`s #78
- `add_features()` is now paginated and performed in parallel. It gains an argument `chunk_size` which determines the maximum number of rows to be added to a feature service.
- adds `get_layers()` which can fetch multiple items from a `FeatureServer` or `MapServer`
  - new utility function `list_items()`
- adds cli as an explicit import (has been implicitly imported by httr2)
- repository made public
- add lifecycle badges to all exported functions <https://github.com/R-ArcGIS/arcgislayers/pull/101>

- **Breaking**: 
  - `token` arguments are required to be a valid `httr2_token` object (strings are not supported).
  - all `host` arguments are removed. Instead, the host is fetched from the `token`.
  - all `user` arguments are removed. Instead, the username is fetched from the `token`. If it is not found, an error is thrown.
