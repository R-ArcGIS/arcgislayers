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


test_that("arc_count() gains fields and crs like arc_select()", {
  select_args <- names(formals(arc_select))
  count_args <- names(formals(arc_count))

  expect_true(all(c("fields", "crs") %in% count_args))
  expect_identical(
    count_args,
    intersect(select_args, count_args)
  )
})

test_that("arc_count() counts distinct values of fields", {
  skip_on_cran()
  furl <- "https://mapprod3.environment.nsw.gov.au/arcgis/rest/services/Planning/EPI_Primary_Planning_Layers/MapServer/2"
  lyr <- arc_open(furl)

  expect_identical(
    arc_count(lyr, fields = "LAY_CLASS", returnDistinctValues = "true"),
    84L
  )
})

test_that("arc_count() validates fields", {
  skip_on_cran()
  expect_error(
    arc_count(arc_open(places_url), fields = "not_a_field"),
    "not_a_field"
  )
})
