test_that("get_layers(): Must be `FeatureServer`, `MapServer` or `GroupLayer`", {
  skip_on_cran()
  furl <- paste0(
    "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/",
    "PLACES_LocalData_for_BetterHealth/FeatureServer"
  )

  fsrv <- arc_open(furl)

  expect_error(get_layers(furl, 0))
})

test_that("get_layers(): Mutually Exclusive", {
  skip_on_cran()
  furl <- paste0(
    "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/",
    "PLACES_LocalData_for_BetterHealth/FeatureServer"
  )

  fsrv <- arc_open(furl)

  expect_error(
    get_layers(fsrv, 0:1, name = c("Tracts", "ZCTAs"))
  )
})

test_that("get_layers(): FeatureServer ID", {
  skip_on_cran()
  furl <- paste0(
    "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/",
    "PLACES_LocalData_for_BetterHealth/FeatureServer"
  )

  fsrv <- arc_open(furl)

  expect_snapshot(
    get_layers(fsrv, 0:1)
  )
})

test_that("get_layers(): FeatureServer name", {
  skip_on_cran()
  furl <- paste0(
    "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/",
    "PLACES_LocalData_for_BetterHealth/FeatureServer"
  )

  fsrv <- arc_open(furl)

  expect_snapshot(
    get_layers(fsrv, name = c("Tracts", "ZCTAs"))
  )
})


test_that("get_layers(): MapServer ID", {
  skip_on_cran()
  murl <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer"
  msrv <- arc_open(murl)

  expect_snapshot(
    get_layers(msrv, 1:2)
  )
})

test_that("get_layers(): MapServer name", {
  skip_on_cran()
  murl <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer"
  msrv <- arc_open(murl)

  expect_snapshot(
    get_layers(msrv, name = c("Census Block Points", "Census Block Group"))
  )
})


test_that("get_layers(): GroupLayer ID", {
  skip_on_cran()
  gurl <- "https://geodata.baltimorecity.gov/egis/rest/services/CitiMap/DOT_Layers/MapServer/0"
  glyr <- arc_open(gurl)

  expect_snapshot(
    get_layers(glyr, 1:2)
  )
})

test_that("get_layers(): GroupLayer name", {
  skip_on_cran()
  gurl <- "https://geodata.baltimorecity.gov/egis/rest/services/CitiMap/DOT_Layers/MapServer/0"
  glyr <- arc_open(gurl)

  expect_snapshot(
    get_layers(glyr, name = c("Bus Stops", "Bus Routes"))
  )
})


test_that("get_layers(): can fetch Table", {
  skip_on_cran()
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Wetlands/FeatureServer"
  fsrv <- arc_open(furl)

  expect_snapshot(get_layers(fsrv, 1))
})
