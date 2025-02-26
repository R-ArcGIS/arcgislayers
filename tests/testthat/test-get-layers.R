furl <- paste0(
  "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/",
  "PLACES_LocalData_for_BetterHealth/FeatureServer"
)

fsrv <- arc_open(furl)

test_that("get_layers(): Must be `FeatureServer`, `MapServer` or `GroupLayer`", {
  skip_on_cran()
  expect_error(get_layers(furl, 0))
})

test_that("get_layers(): Mutually Exclusive", {
  expect_error(
    get_layers(fsrv, 0:1, name = c("Tracts", "ZCTAs"))
  )
})

test_that("get_layers(): FeatureServer ID", {
  expect_snapshot(
    get_layers(fsrv, 0:1)
  )
})

test_that("get_layers(): FeatureServer name", {
  expect_snapshot(
    get_layers(fsrv, name = c("Tracts", "ZCTAs"))
  )
})


murl <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer"
msrv <- arc_open(murl)

test_that("get_layers(): MapServer ID", {
  expect_snapshot(
    get_layers(msrv, 1:2)
  )
})

test_that("get_layers(): MapServer name", {
  expect_snapshot(
    get_layers(msrv, name = c("Census Block Points", "Census Block Group"))
  )
})

gurl <- "https://geodata.baltimorecity.gov/egis/rest/services/CitiMap/DOT_Layers/MapServer/0"
glyr <- arc_open(gurl)

test_that("get_layers(): GroupLayer ID", {
  expect_snapshot(
    get_layers(glyr, 1:2)
  )
})

test_that("get_layers(): GroupLayer name", {
  expect_snapshot(
    get_layers(glyr, name = c("Bus Stops", "Bus Routes"))
  )
})


test_that("get_layers(): can fetch Table", {
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Wetlands/FeatureServer"
  fsrv <- arc_open(furl)

  expect_snapshot(get_layers(fsrv, 1))
})
