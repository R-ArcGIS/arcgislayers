nhd_url <- file.path(
  "https://hydro.nationalmap.gov",
  "arcgis/rest/services",
  "NHDPlus_HR",
  "MapServer"
)

test_that("map servers can be opened", {
  skip_on_cran()
  nhd_srv <- arc_open(nhd_url)

  expect_s3_class(nhd_srv, "MapServer")
  expect_identical(
    nhd_srv[["spatialReference"]][["latestWkid"]],
    3857L
  )

  layers <- nhd_srv[["layers"]]
  expect_identical(nrow(layers), 13L)
  expect_identical(layers[["id"]], 0:12)
  expect_identical(layers[["name"]][1], "NHDPlusGage")
  expect_identical(layers[["name"]][13], "WBDHU12")
  expect_identical(layers[["geometryType"]][1], "esriGeometryPoint")
  expect_identical(layers[["geometryType"]][13], "esriGeometryPolygon")
})


test_that("layers can be retrieved from mapserver's", {
  skip_on_cran()
  nhd_srv <- arc_open(nhd_url)

  expect_no_error(get_layer(nhd_srv, 0))
  expect_no_error(get_layers(nhd_srv, id = c(0, 2)))
})
