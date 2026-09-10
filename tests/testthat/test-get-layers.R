places_furl <- paste0(
  "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/",
  "PLACES_LocalData_for_BetterHealth/FeatureServer"
)

census_murl <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer"

dot_gurl <- "https://geodata.baltimorecity.gov/egis/rest/services/CitiMap/DOT_Layers/MapServer/0"

wetlands_furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Wetlands/FeatureServer"

test_that("get_layers(): Must be `FeatureServer`, `MapServer` or `GroupLayer`", {
  skip_on_cran()
  expect_error(get_layers(places_furl, 0))
})

test_that("get_layers(): Mutually Exclusive", {
  skip_on_cran()
  fsrv <- arc_open(places_furl)

  expect_error(
    get_layers(fsrv, 0:1, name = c("Tracts", "ZCTAs"))
  )
})

test_that("get_layers(): FeatureServer ID", {
  skip_on_cran()
  lyrs <- get_layers(arc_open(places_furl), 0:1)

  expect_length(lyrs, 2)
  expect_layer(lyrs[[1]], "PlacePoints", "esriGeometryPoint", 3785L)
  expect_layer(lyrs[[2]], "PlaceBoundaries", "esriGeometryPolygon", 3785L)
})

test_that("get_layers(): FeatureServer name", {
  skip_on_cran()
  lyrs <- get_layers(arc_open(places_furl), name = c("Tracts", "ZCTAs"))

  expect_length(lyrs, 2)
  expect_layer(lyrs[[1]], "Tracts", "esriGeometryPolygon", 3785L)
  expect_layer(lyrs[[2]], "ZCTAs", "esriGeometryPolygon", 3785L)
})


test_that("get_layers(): MapServer ID", {
  skip_on_cran()
  lyrs <- get_layers(arc_open(census_murl), 1:2)

  expect_length(lyrs, 2)
  expect_layer(lyrs[[1]], "Census Block Group", "esriGeometryPolygon", 4269L)
  expect_layer(lyrs[[2]], "Detailed Counties", "esriGeometryPolygon", 4269L)
})

test_that("get_layers(): MapServer name", {
  skip_on_cran()
  lyrs <- get_layers(
    arc_open(census_murl),
    name = c("Census Block Points", "Census Block Group")
  )

  expect_length(lyrs, 2)
  expect_layer(lyrs[[1]], "Census Block Points", "esriGeometryPoint", 4269L)
  expect_layer(lyrs[[2]], "Census Block Group", "esriGeometryPolygon", 4269L)
})


test_that("get_layers(): GroupLayer ID", {
  skip_on_cran()
  lyrs <- get_layers(arc_open(dot_gurl), 1:2)

  expect_length(lyrs, 2)
  expect_layer(lyrs[[1]], "Bus Stops", "esriGeometryPoint", 2248L)
  expect_layer(lyrs[[2]], "Bus Routes", "esriGeometryPolyline", 2248L)
})

test_that("get_layers(): GroupLayer name", {
  skip_on_cran()
  lyrs <- get_layers(arc_open(dot_gurl), name = c("Bus Stops", "Bus Routes"))

  expect_length(lyrs, 2)
  expect_layer(lyrs[[1]], "Bus Stops", "esriGeometryPoint", 2248L)
  expect_layer(lyrs[[2]], "Bus Routes", "esriGeometryPolyline", 2248L)
})


test_that("get_layers(): can fetch Table", {
  skip_on_cran()
  lyrs <- get_layers(arc_open(wetlands_furl), 1)

  expect_length(lyrs, 1)
  expect_s3_class(lyrs[[1]], "Table")
  expect_identical(lyrs[[1]][["name"]], "Pop_Up_Table")
})
