test_that("arc_select(): polygons can be parsed", {

  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"
  flayer <- arc_open(furl)

  expect_no_error(arc_select(flayer))

})


test_that("arc_select(): tables can be parsed", {

  furl <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/List_of_Providers/FeatureServer/27"

  tblayer <- arc_open(furl)
  expect_no_error(arc_select(tblayer))
})


test_that("arc_select() works on `ImageServer`s", {
  img_url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"

  landsat <- arc_open(img_url)

  tmp <- arc_select(landsat, n_max = 2, where = "Month = 2")
  expect_snapshot(tmp)
})
