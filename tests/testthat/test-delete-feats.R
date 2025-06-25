test_that("deleting features works", {
  skip("Must be ran interactively")
  skip_if_not_installed("sf")

  # ensure that NC test is published first
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  set_arc_token(auth_user())
  res <- publish_layer(nc, "NC test")

  nc_layer <- get_layer(arc_open(res$services$encodedServiceURL), 0)

  # define a bounding box that we use to delete geometries that touch it.
  bbox_to_delete <- sf::st_bbox(nc$geometry[1])

  # delete based on bbox
  del_res <- delete_features(nc_layer, filter_geom = bbox_to_delete)

  expect_identical(nrow(del_res), 4L)
  expect_true(all(del_res$success))

  # delete based on object ids
  del_res <- delete_features(nc_layer, 3:10)
  expect_identical(nrow(del_res), 8L)
  expect_true(all(del_res$success))

  # delete based on where clause
  del_res <- delete_features(nc_layer, where = "CNTY_ < 1850")
  expect_identical(nrow(del_res), 7L)
  expect_true(all(del_res$success))
})
