test_that("Adding features to a table work", {

  skip("Must be ran manually")

  set_auth_token(auth_code())

  # ensure that Iris Test exists first
  publish_layer(iris, "Iris Test")

  irs <- arc_open("https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/Iris%20Test/FeatureServer/0")

  test_row <- data.frame(
    Sepal.Length = 1:5,
    Sepal.Width = 2:6,
    Petal.Length = 3:7,
    Petal.Width = 4:8,
    Species = "ArcGIS Iris"
  )

  expect_success(add_features(irs, test_row))

})


test_that("Adding features to a feature layer works", {
  skip("Must be ran interactively")
  skip_if_not_installed("sf")

  # ensure that NC test is published first
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  tkn <- auth_code()
  set_auth_token(tkn)
  res <- publish_layer(nc, "NC test")

  nc_layer <- arc_open("https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/NC%20test/FeatureServer/0")

  nc$geometry <- sf::st_convex_hull(nc$geometry)
  nc <- sf::st_transform(nc, 3857)

  res <- add_features(nc_layer, nc)


})
