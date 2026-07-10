test_that("as_feature_collection_json() produces stable JSON", {
  x <- subset(penguins, is.na(sex))
  title <- "penguin-test"

  json <- as_feature_collection_json(
    list(arcgisutils::as_layer(x, title, title))
  )

  expect_snapshot(json)
})
