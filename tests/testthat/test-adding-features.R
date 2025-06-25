test_that("Adding feature and updating them to a table work", {
  skip("Must be ran interactively")
  skip_if_not_installed("dplyr")

  set_arc_token(auth_user())

  # ensure that Iris Test exists first
  res <- publish_layer(iris, ulid::ulid())

  irs <- arc_open(file.path(res$services$encodedServiceURL, "0"))

  test_row <- data.frame(
    Sepal.Length = 1:5,
    Sepal.Width = 2:6,
    Petal.Length = 3:7,
    Petal.Width = 4:8,
    Species = "ArcGIS Iris"
  )

  expect_no_error(add_features(irs, test_row, match_on = "alias"))

  # create a massive data frame
  test_df <- dplyr::as_tibble(
    dplyr::sample_n(iris, 4500, replace = TRUE)
  ) |>
    dplyr::mutate(
      Species = sample(
        c("Hi", "Howdy", "Esri", "Ft"),
        dplyr::n(),
        replace = TRUE
      )
    )

  expect_no_error(add_features(irs, test_df, match_on = "alias"))

  # collect the feature service
  to_update <- arc_select(irs) |>
    dplyr::transmute(
      object_id = as.integer(object_id),
      Sepal_Length = Sepal_Length * 1.05
    )

  # update all of these
  update_res <- update_features(irs, to_update, chunk_size = 25)
})


test_that("Adding features to a feature layer works", {
  skip("Must be ran interactively")
  skip_if_not_installed("sf")

  # ensure that NC test is published first
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  tkn <- auth_code()
  set_arc_token(tkn)
  res <- publish_layer(nc, "NC test")

  nc_layer <- get_layer(arc_open(res$services$encodedServiceURL), 0)

  nc$geometry <- sf::st_convex_hull(nc$geometry)

  res <- add_features(nc_layer, nc)
})


test_that("truncate_layer() works:", {
  skip("Must be ran interactively")

  truncate_layer(nc_layer)
})
