test_that("map servers can be opened", {
  skip_on_cran()

  nhd <- file.path(
    "https://hydro.nationalmap.gov",
    "arcgis/rest/services",
    "NHDPlus_HR",
    "MapServer"
  )

  nhd_srv <- arc_open(nhd)
  expect_snapshot(nhd_srv)
})


test_that("layers can be retrieved from mapserver's", {
  skip_on_cran()

  nhd <- file.path(
    "https://hydro.nationalmap.gov",
    "arcgis/rest/services",
    "NHDPlus_HR",
    "MapServer"
  )

  nhd_srv <- arc_open(nhd)

  expect_no_error(get_layer(nhd_srv, 0))
  expect_no_error(get_layers(nhd_srv, id = c(0, 2)))
})
