# arcgislayers 0.1.0 (unreleased)

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
