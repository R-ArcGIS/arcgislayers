# arcgislayers 0.1.0 (unreleased)

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
- Use `arcgisutils::arc_token()` to get "ARCGIS_TOKEN" environment variable. This ensures that empty strings do not cause HTTP 498 "invalid token" error.
