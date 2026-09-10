fake_layer <- function() {
  structure(
    list(
      capabilities = "Query",
      fields = data.frame(name = "A", type = "esriFieldTypeString"),
      spatialReference = list(wkid = 4326L)
    ),
    class = c("FeatureLayer", "list"),
    query = list()
  )
}

test_that("arc_select() catches misspelled arguments (#226)", {
  expect_error(arc_select(fake_layer(), wher = "1=1"), "where")
  expect_error(arc_select(fake_layer(), field = "A"), "fields")
  expect_error(arc_select(fake_layer(), n_mx = 1), "n_max")
})

test_that("arc_select() catches wrong case (#226)", {
  expect_error(arc_select(fake_layer(), Where = "1=1"), "where")
  expect_error(arc_select(fake_layer(), CRS = 4326), "crs")
})

test_that("arc_select() names the offending argument (#226)", {
  expect_error(arc_select(fake_layer(), wher = "1=1"), "wher")
})

test_that("arc_select() still allows Esri query parameters (#226)", {
  for (nm in c(
    "outSR",
    "resultType",
    "orderByFields",
    "returnDistinctValues"
  )) {
    dots <- setNames(list("1"), nm)
    expect_no_error(do.call(check_dots_query_names, list(names(dots))))
  }
})

test_that("check_dots_query_names() tolerates no dots (#226)", {
  expect_no_error(check_dots_query_names(character()))
  expect_no_error(check_dots_query_names(NULL))
})
