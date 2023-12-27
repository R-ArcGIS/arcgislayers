test_that("arc_read(): FeatureServer can be read", {
  skip_on_cran()
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

  layer <- arc_read(furl)

  # if any errors occur above here the test will fail
  expect_true(TRUE)
})


test_that("arc_read(): ImageServer can be read", {
  skip_on_cran()
  img_url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"

  res <- arc_read(
    img_url,
    xmin = -71,
    ymin = 43,
    xmax = -67,
    ymax = 47.5,
    crs = 4326,
    height = 50,
    width = 50
  )

  expect_s4_class(res, "SpatRaster")
  expect_equal(attr(class(res), "package"), "terra")

})


test_that("arc_read(): name_repair works", {
  skip_on_cran()
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

test_that("arc_read(): n_max is correct", {
  skip_on_cran()
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

  expect_equal(nrow(arc_read(furl, n_max = 1)), 1L)
  expect_equal(nrow(arc_read(furl, n_max = 1234)), 1234L)

})


test_that("arc_read(): n_max option is respected", {
  skip_on_cran()
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

  # set n_max via options
  options("arcgislayers.n_max" = 1234)

  layer <- arc_read(furl)
  expect_equal(nrow(layer), 1234L)
})

test_that("arc_read(): n_max option is ignored when n_max is set", {
  skip_on_cran()
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

  # set n_max via options
  options("arcgislayers.n_max" = 1234)

  layer <- arc_read(furl, n_max = 321)
  expect_equal(nrow(layer), 321L)
})

test_that("arc_read(): correct error with unsupported type", {
  skip_on_cran()
  furl <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer"
  expect_error(arc_read(furl), "is not a supported type")
})
