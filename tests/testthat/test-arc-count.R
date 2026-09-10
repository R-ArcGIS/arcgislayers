places_url <- "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/PLACES_LocalData_for_BetterHealth/FeatureServer/0"

test_that("arc_count() returns a single count (#293)", {
  skip_on_cran()
  n <- arc_count(arc_open(places_url))

  expect_length(n, 1L)
  expect_true(is.numeric(n))
  expect_gt(n, 0)
})

test_that("arc_count() respects where (#293)", {
  skip_on_cran()
  flayer <- arc_open(places_url)

  expect_lt(
    arc_count(flayer, where = "StateAbbr = 'RI'"),
    arc_count(flayer)
  )
})

test_that("arc_count() agrees with arc_select() (#293)", {
  skip_on_cran()
  flayer <- arc_open(places_url)
  where <- "StateAbbr = 'RI'"

  expect_identical(
    as.integer(arc_count(flayer, where = where)),
    nrow(arc_select(flayer, where = where, fields = "StateAbbr"))
  )
})

test_that("arc_count() returns 0 for an empty result (#293)", {
  skip_on_cran()
  expect_identical(arc_count(arc_open(places_url), where = "1 = 0"), 0L)
})

test_that("arc_count() rejects a non layer (#293)", {
  expect_error(arc_count(data.frame()), "FeatureLayer")
})

test_that("arc_count() catches misspelled arguments (#293)", {
  expect_error(arc_count(arc_open(places_url), wher = "1=1"), "where")
})
