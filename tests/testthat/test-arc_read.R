test_that("arc_read(): FeatureServer can be read", {

  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

  layer <- arc_read(furl)

  # if any errors occur above here the test will fail
  expect_true(TRUE)
})


test_that("arc_read(): ImageServer can be read", {

  img_url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"

  res <- arc_read(
    img_url,
    xmin = -71,
    ymin = 43,
    xmax = -67,
    ymax = 47.5,
    crs = 4326,
    height = 100,
    width = 100
  )

  expect_s4_class(res, "SpatRaster")
  expect_equal(attr(class(res), "package"), "terra")

})


test_that("arc_read(): name_repair works", {

  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

  col_select <- c("NAME", "FIPS")

  layer <- arc_read(furl, col_select = col_select, name_repair = tolower)

  expect_named(layer, c("name", "fips", "geometry"))

  layer <- arc_read(furl, col_select = col_select, col_names = c("Name", "FIPS Code"))

  expect_named(layer, c("Name", "FIPS Code", "geometry"))

  expect_error(
    arc_read(furl, col_select = col_select, col_names = c("Name", "Name"), name_repair = "check_unique")
  )

})
