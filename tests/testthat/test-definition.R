test_that("update_definition works", {
  skip("Must be ran interactively")
  # set auth token
  set_arc_token(auth_code())

  # ensure that Iris Test exists first
  # publish_layer(iris, "Iris Test")

  irs <- arc_open(
    "https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/Iris%20Test/FeatureServer/0"
  )

  existing_desc <- irs[["description"]]
  update_desc <- "Updated description for update_definition test."

  expect_success(
    {
      irs_update <- update_definition(
        irs,
        description = update_desc
      )
    }
  )

  expect_identical(irs_update$description, update_desc)

  # Clean-up after test by restoring existing description
  update_definition(irs_update, description = existing_desc)
})
