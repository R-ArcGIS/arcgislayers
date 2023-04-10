test_that("arc_select(): polygons can be parsed", {

  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"
  flayer <- arc_open(furl)

  arc_select(flayer)

  # if any errors occur above here the test will fail
  expect_true(TRUE)
})


test_that("arc_select(): tables can be parsed", {

  furl <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/List_of_Providers/FeatureServer/27"

  tblayer <- arc_open(furl)

  arc_select(tblayer)

  # if any errors occur above here the test will fail
  expect_true(TRUE)
})
