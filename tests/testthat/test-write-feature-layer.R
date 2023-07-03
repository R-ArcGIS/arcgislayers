test_that("Feature Layer can be written from an sf object ", {
  skip("Must be ran interactively")
  skip_if_not_installed("sf")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  tkn <- auth_code()
  set_auth_token(tkn)
  res <- publish_layer(nc, "NC test")

  expect_null(res$services$success)
})


test_that("Table can be written from an sf object ", {
  skip("Must be ran interactively")
  tkn <- auth_code()
  set_auth_token(tkn)
  res <- publish_layer(iris, "Iris Test")
  expect_null(res$services$success)
})
