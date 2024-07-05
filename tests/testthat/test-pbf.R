test_that("supports pbf: polygons", {
  furl <- "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/ArcGIS/rest/services/PLACES_LocalData_for_BetterHealth/FeatureServer/1"
  x <- arc_open(furl)
  expect_no_error(arc_select(x, where = "stateabbr = 'GA' and pop2010 > 100000"))
})

test_that("supports pbf: points", {
  x <- arc_open("https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Major_Cities_/FeatureServer/0")
  expect_no_error(arc_select(x, n_max = 1000L))
})

test_that("does not support pbf: multilinestring", {
  furl <- "https://egisp.dot.ga.gov/arcgis/rest/services/ARCWEBSVCMAP/MapServer/1"
  x <- arc_open(furl)
  expect_no_error(arc_select(x, n_max = 10))
})
