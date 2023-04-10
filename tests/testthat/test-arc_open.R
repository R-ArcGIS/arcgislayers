# Note that all expectations are expect_true(TRUE)
# if an error happens before hand the test will fail.
# These tests test that the service can be read correctly.
# can be improved to check attributes like query, class, etc.

test_that("arc_open: Feature Layer", {
  ft_url <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

  arc_open(ft_url)

  expect_true(TRUE)

})


test_that("arc_open: Table", {

  tbl_url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/List_of_Providers/FeatureServer/27"

  arc_open(tbl_url)

  expect_true(TRUE)
})

test_that("arc_open: Feature Server", {

  server_url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/hexagons_state/FeatureServer"

  arc_open(server_url)

  expect_true(TRUE)

})



test_that("arc_open: Image Server", {
  img_url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"

  arc_open(img_url, token = "") # hella weird behavior with token here.

  expect_true(TRUE)

})








