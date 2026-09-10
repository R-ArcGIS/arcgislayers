hex_server_url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/hexagons_state/FeatureServer"

census_map_url <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/"

dot_group_url <- "https://geodata.baltimorecity.gov/egis/rest/services/CitiMap/DOT_Layers/MapServer/0"

test_that("get_all_layers(): Must be `FeatureServer`, `MapServer` or `GroupLayer`", {
  skip_on_cran()
  expect_error(get_all_layers(hex_server_url, 0))
})


test_that("get_all_layers(): FeatureServer", {
  skip_on_cran()
  res <- get_all_layers(arc_open(hex_server_url))

  expect_named(res, "layers")
  expect_named(res[["layers"]], c("0", "1", "2"))
  expect_layer(res[["layers"]][["0"]], "states_hex", "esriGeometryPolygon", 4326L)
  expect_layer(res[["layers"]][["1"]], "states_con", "esriGeometryPolygon", 4326L)
  expect_layer(res[["layers"]][["2"]], "hexagons", "esriGeometryPolygon", 4326L)
})


test_that("get_all_layers(): MapLayer", {
  skip_on_cran()
  res <- get_all_layers(arc_open(census_map_url))

  expect_named(res, "layers")
  expect_named(res[["layers"]], c("0", "1", "2", "3"))
  expect_layer(res[["layers"]][["0"]], "Census Block Points", "esriGeometryPoint", 4269L)
  expect_layer(res[["layers"]][["1"]], "Census Block Group", "esriGeometryPolygon", 4269L)
  expect_layer(res[["layers"]][["2"]], "Detailed Counties", "esriGeometryPolygon", 4269L)
  expect_layer(res[["layers"]][["3"]], "states", "esriGeometryPolygon", 4269L)
})


test_that("get_all_layers(): GroupLayer", {
  skip_on_cran()
  res <- get_all_layers(arc_open(dot_group_url))

  expect_length(res, 2)
  expect_layer(res[[1]], "Bus Stops", "esriGeometryPoint", 2248L)
  expect_layer(res[[2]], "Bus Routes", "esriGeometryPolyline", 2248L)
})
