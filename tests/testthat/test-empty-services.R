test_that("empty feature services returns empty df", {
  skip_on_cran()
  furl <- "https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/test-empty/FeatureServer/0"

  layer <- arc_open(furl)

  feats <- arc_select(layer)
  expect_identical(
    arcgisutils::fields_as_ptype_df(list_fields(layer)),
    feats
  )
})
