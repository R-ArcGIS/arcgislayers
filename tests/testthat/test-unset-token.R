test_that("Error reported when token is required (FeatureServer)", {
  fsurl <- "https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/Madagascar_points/FeatureServer"

  expect_error(arc_open(fsurl), "Status code: 499")
})

test_that("Error reported for invalid url (FeatureLayer)", {
  fsurl <- "https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/Madagascar_points/FeatureServer/0"

  expect_error(arc_open(fsurl), "Status code: 499")
})
