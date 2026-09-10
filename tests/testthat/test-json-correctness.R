test_that("as_feature_collection_json() produces stable JSON", {
  x <- subset(penguins, is.na(sex))
  title <- "penguin-test"

  json <- as_feature_collection_json(
    list(arcgisutils::as_layer(x, title, title))
  )

  parsed <- RcppSimdJson::fparse(json)
  layers <- parsed[["layers"]]
  defn <- layers[["layerDefinition"]][[1]]

  expect_true(parsed[["showLegend"]])
  expect_identical(nrow(layers), 1L)
  expect_identical(layers[["name"]], title)
  expect_identical(layers[["title"]], title)
  expect_identical(defn[["name"]], title)
  expect_identical(defn[["objectIdField"]], "object_id")
  expect_identical(defn[["type"]], "Table")
  expect_false(defn[["hasAttachments"]])

  expect_identical(
    defn[["fields"]][["name"]],
    c(
      "object_id",
      "species",
      "island",
      "bill_len",
      "bill_dep",
      "flipper_len",
      "body_mass",
      "sex",
      "year"
    )
  )

  expect_identical(
    defn[["fields"]][["type"]],
    c(
      "esriFieldTypeOID",
      "esriFieldTypeString",
      "esriFieldTypeString",
      "esriFieldTypeDouble",
      "esriFieldTypeDouble",
      "esriFieldTypeInteger",
      "esriFieldTypeInteger",
      "esriFieldTypeString",
      "esriFieldTypeInteger"
    )
  )

  attributes <- layers[["featureSet"]][[1]][["features"]][["attributes"]]
  expect_length(attributes, nrow(x))
  expect_identical(
    vapply(attributes, `[[`, numeric(1), "object_id"),
    as.numeric(seq_len(nrow(x)))
  )
  expect_true(all(vapply(attributes, function(a) is.null(a[["sex"]]), logical(1))))
})
