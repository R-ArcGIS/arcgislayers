test_that("page size is resepected", {
  skip_on_cran()

  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Census_2020_Redistricting_Blocks/FeatureServer/0"

  layer <- arc_open(furl)
  expect_no_error(arc_select(layer, n_max = 10, page_size = 2))
})
