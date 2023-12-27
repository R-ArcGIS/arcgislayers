test_that("arc_select(): geometry flag works", {

  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"
  flayer <- arc_open(furl)

  res <- arc_select(flayer, n_max = 3, geometry = FALSE)
  expect_false("sf" %in% class(res))

  res <- arc_select(flayer, n_max = 3)
  expect_true("sf" %in% class(res))
})

test_that("arc_select(): ignores geometry for tables", {

  furl <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/List_of_Providers/FeatureServer/27"

  flayer <- arc_open(furl)

  res <- arc_select(flayer, n_max = 3, geometry = FALSE)
  expect_false("sf" %in% class(res))

  res <- arc_select(flayer, n_max = 3, geometry = TRUE)
  expect_false("sf" %in% class(res))
})
