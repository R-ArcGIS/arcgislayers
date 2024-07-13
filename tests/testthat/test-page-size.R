test_that("multiplication works", {
  furl <- "https://services.arcgis.com/GL0fWlNkwysZaKeV/arcgis/rest/services/TXLA_ZCTA_PRCPpred/FeatureServer/0"

  layer <- arc_open(furl)
  expect_no_error(arc_select(layer, n_max = 10, page_size = 2))
})
