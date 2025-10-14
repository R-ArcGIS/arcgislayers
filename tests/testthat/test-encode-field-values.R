test_that("encode_field_values() encodes field values", {
  skip_on_cran()
  layer <- arc_open(
    "https://geodata.baltimorecity.gov/egis/rest/services/Housing/dmxOwnership/MapServer/0"
  )

  res <- arc_select(layer, n_max = 100, where = "RESPAGCY <> '  '")
  encoded <- encode_field_values(res, layer)

  # get unique encoded vals
  encoded_vals <- sort(unique(encoded$RESPAGCY))

  # fetch domains and known values
  domains <- list_field_domains(layer)
  domain_vals <- domains[[c("RESPAGCY", "codedValues", "name")]]

  expect_true(all(encoded_vals %in% domain_vals))
})

test_that("encode_field_values() encodes field values when field is set", {
  skip_on_cran()
  flayer <- arc_open(
    "https://services1.arcgis.com/99lidPhWCzftIe9K/ArcGIS/rest/services/UtahRoads/FeatureServer/0"
  )

  res <- arc_select(
    flayer,
    fields = c("CARTOCODE", "SPEED_LMT"),
    n_max = 10,
    geometry = FALSE
  )

  encoded <- encode_field_values(res, flayer, field = "CARTOCODE")

  # fetch domains and known values
  domains <- list_field_domains(flayer, field = "CARTOCODE")
  domain_vals <- domains[[c("CARTOCODE", "codedValues", "name")]]

  expect_true(all(encoded[["CARTOCODE"]] %in% domain_vals))

  # create incorrect values
  res$SPEED_LMT[2:5] <- 17L
  # expect message for field w/ invalid codes
  expect_message(
    encode_field_values(res, flayer, field = "SPEED_LMT")
  )

  # expect no message for field w/ invalid codes if `codes = "replace-valid"`
  expect_no_message(
    encode_field_values(
      res,
      flayer,
      field = "SPEED_LMT",
      codes = "replace-valid"
    )
  )
})
