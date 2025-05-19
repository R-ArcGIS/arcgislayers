test_that("get_all_layers(): Must be `FeatureServer`, `MapServer` or `GroupLayer`", {
  skip_on_cran()
  server_url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/hexagons_state/FeatureServer"
  expect_error(get_all_layers(server_url, 0))
})


test_that("get_all_layers(): FeatureServer", {
  skip_on_cran()
  server_url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/hexagons_state/FeatureServer"

  fsrv <- arc_open(server_url)
  expect_snapshot(get_all_layers(fsrv))
})


test_that("get_all_layers(): MapLayer", {
  skip_on_cran()
  murl <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/"
  msrv <- arc_open(murl)
  expect_snapshot(get_all_layers(msrv))
})


test_that("get_all_layers(): GroupLayer", {
  skip_on_cran()
  gurl <- "https://geodata.baltimorecity.gov/egis/rest/services/CitiMap/DOT_Layers/MapServer/0"
  glyr <- arc_open(gurl)
  expect_snapshot(get_all_layers(glyr))
})
