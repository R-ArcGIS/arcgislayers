# FeatureServer -----------------------------------------------------------

hex_url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/hexagons_state/FeatureServer"

imagery_url <- paste0(
  "https://services.arcgisonline.com/ArcGIS/rest/services/",
  "World_Imagery/MapServer"
)

dot_url <- "https://geodata.baltimorecity.gov/egis/rest/services/CitiMap/DOT_Layers/MapServer/0"

test_that("get_layer(): Must be `FeatureServer`, `MapServer` or `GroupLayer`", {
  skip_on_cran()
  expect_error(get_layer(hex_url, 0))
})

test_that("get_layer(): Generic - name and id are mutually exclusive", {
  skip_on_cran()
  fsrv <- arc_open(hex_url)

  expect_error(get_layer(fsrv, 0, "break"))
})

test_that("get_layer(): `FeatureServer` by ID", {
  skip_on_cran()
  fsrv <- arc_open(hex_url)

  expect_layer(
    get_layer(fsrv, 0),
    "states_hex",
    "esriGeometryPolygon",
    4326L
  )
})

test_that("get_layer(): `FeatureServer` by name", {
  skip_on_cran()
  fsrv <- arc_open(hex_url)

  expect_layer(
    get_layer(fsrv, name = "states_hex"),
    "states_hex",
    "esriGeometryPolygon",
    4326L
  )
})


# MapServer ---------------------------------------------------------------

test_that("get_layer(): `MapServer` by ID", {
  skip_on_cran()
  msrv <- arc_open(imagery_url)

  expect_layer(
    get_layer(msrv, 3),
    "High Resolution 30cm Imagery",
    "esriGeometryPolygon",
    3857L
  )
})

test_that("get_layer(): `MapServer` by name", {
  skip_on_cran()
  msrv <- arc_open(imagery_url)

  expect_layer(
    get_layer(msrv, name = "Citations"),
    "Citations",
    "esriGeometryPolygon",
    3857L
  )
})


# GroupLayer --------------------------------------------------------------

test_that("get_layer(): `GroupLayer` by ID", {
  skip_on_cran()
  glyr <- arc_open(dot_url)

  expect_layer(
    get_layer(glyr, 2),
    "Bus Routes",
    "esriGeometryPolyline",
    2248L
  )
})

test_that("get_layer(): `GroupLayer` by name", {
  skip_on_cran()
  glyr <- arc_open(dot_url)

  expect_layer(
    get_layer(glyr, name = "Bus Stops"),
    "Bus Stops",
    "esriGeometryPoint",
    2248L
  )
})
