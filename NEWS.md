# arcgislayers (development)

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
