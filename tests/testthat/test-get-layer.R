# FeatureServer -----------------------------------------------------------

# feature server
server_url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/hexagons_state/FeatureServer"

fsrv <- arc_open(server_url)

test_that("get_layer(): Generic - name and id are mutually exclusive", {
  skip_on_cran()
  expect_error(get_layer(srv, 0, "break"))
})


test_that("get_layer(): `FeatureServer` by ID", {
  skip_on_cran()
  expect_snapshot(get_layer(fsrv, 0))
})

test_that("get_layer(): `FeatureServer` by name", {
  skip_on_cran()
  expect_snapshot(get_layer(fsrv, name = "states_hex"))
})


# MapServer ---------------------------------------------------------------

map_url <- paste0(
  "https://services.arcgisonline.com/ArcGIS/rest/services/",
  "World_Imagery/MapServer"
)

msrv <- arc_open(map_url)

# map server
test_that("get_layer(): `MapServer` by ID", {
  skip_on_cran()
  expect_snapshot(get_layer(msrv, 3))
})

test_that("get_layer(): `MapServer` by name", {
  expect_snapshot(get_layer(msrv, name = "Citations"))
})


# GroupLayer --------------------------------------------------------------
gurl <- "https://geodata.baltimorecity.gov/egis/rest/services/CitiMap/DOT_Layers/MapServer/0"
glyr <- arc_open(gurl)


test_that("get_layer(): `GroupLayer` by ID", {
  skip_on_cran()
  expect_snapshot(get_layer(glyr, 2))
})

test_that("get_layer(): `GroupLayer` by name", {
  skip_on_cran()
  expect_snapshot(get_layer(glyr, name = "Bus Stops"))
})
