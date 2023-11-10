test_that("arc_select(): CRS is respected", {
  url <- "https://services1.arcgis.com/UWYHeuuJISiGmgXx/arcgis/rest/services/Police_District/FeatureServer/0"

  epsg_code <- 2804

  flayer <- arc_open(url)

  res <- arc_select(flayer, crs = epsg_code, n_max = 1)

  expect_identical(
    sf::st_crs(res),
    sf::st_crs(epsg_code)
  )
})


