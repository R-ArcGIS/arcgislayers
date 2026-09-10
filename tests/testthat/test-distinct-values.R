places_url <- "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/PLACES_LocalData_for_BetterHealth/FeatureServer/0"

test_that("returnDistinctValues de-duplicates a field (#233)", {
  skip_on_cran()
  flayer <- arc_open(places_url)

  res <- arc_select(
    flayer,
    fields = "StateAbbr",
    geometry = FALSE,
    returnDistinctValues = TRUE
  )

  expect_identical(nrow(res), length(unique(res$StateAbbr)))
  expect_gt(nrow(res), 1)
})

test_that("returnDistinctValues returns fewer rows than the full query (#233)", {
  skip_on_cran()
  flayer <- arc_open(places_url)

  distinct <- arc_select(
    flayer,
    fields = "StateAbbr",
    geometry = FALSE,
    returnDistinctValues = TRUE
  )

  expect_lt(nrow(distinct), arc_count(flayer))
})
