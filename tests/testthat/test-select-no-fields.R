test_that("arc_select() returns geometry only for a layer (#248)", {
  skip_on_cran()
  furl <- "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/PLACES_LocalData_for_BetterHealth/FeatureServer/0"
  flayer <- arc_open(furl)

  res <- arc_select(flayer, fields = "", n_max = 10)

  expect_s3_class(res, "sf")
  expect_identical(nrow(res), 10L)
  expect_identical(names(res), attr(res, "sf_column"))
})

test_that("arc_select() returns rows without columns for a table (#248)", {
  skip_on_cran()
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Wetlands/FeatureServer/1"
  tblayer <- arc_open(furl)

  res <- arc_select(tblayer, fields = "", n_max = 100)

  expect_identical(dim(res), c(100L, 0L))
})

test_that("arc_select() still reports genuinely empty results (#248)", {
  skip_on_cran()
  furl <- "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/PLACES_LocalData_for_BetterHealth/FeatureServer/0"
  flayer <- arc_open(furl)

  expect_message(
    res <- arc_select(flayer, where = "1 = 0"),
    "No features returned"
  )
  expect_identical(nrow(res), 0L)
})
