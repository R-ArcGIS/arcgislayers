test_that("Table fields can be updated", {


  skip("Must be ran interactively")
  skip_if_not_installed("dplyr")
  # set auth token
  set_auth_token(auth_code())

  # ensure that Iris Test exists first
  # publish_layer(iris, "Iris Test")

  irs <- arc_open("https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/Iris%20Test/FeatureServer/0")

  .df <- arc_select(irs)


  y <- dplyr::filter(.df, Species == "versicolor") |>
    dplyr::mutate(Sepal_Length = 999)


  expect_success(update_features(irs, y))

})


test_that("Feature Layer fields can be updated", {
  skip("Must be ran interactively")
  skip_if_not_installed("sf")

  # ensure that NC test is published first
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

  # set auth token
  set_auth_token(auth_code())

  # ensure the layer exists
  # publish_layer(nc, "NC test")

  nc_layer <- arc_open("https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/NC%20test/FeatureServer/0")

  ncsf <- arc_select(nc_layer)

  # modify name
  ncsf$NAME <- paste0("County name: ", toupper(ncsf$NAME))

  # extract only object id and the new county name field
  y <- as.data.frame(ncsf)[, c("object_id", "NAME")]

  expect_success(update_features(nc_layer, y))
})
