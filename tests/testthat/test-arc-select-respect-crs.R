test_that("arc_select(): CRS is respected", {
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

  epsg_code <- 2804

  flayer <- arc_open(furl)

  res <- arc_select(flayer, crs = epsg_code, n_max = 1)

  expect_identical(
    sf::st_crs(res),
    sf::st_crs(epsg_code)
  )
})
