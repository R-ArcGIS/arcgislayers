library(sf)

test_that("st_envelope(): sfc_POINT xy", {
  skip_if_not_installed("sfheaders")
  library(sfheaders)

  set.seed(0)
  m <- matrix(runif(200, -180, 180), ncol = 2)
  pnts_sfc <- sfc_point(m)

  env <- st_envelope(pnts_sfc)

  # check that its an envelopp
  expect_s3_class(env, "envelope")
  # check that all are not missing
  expect_true(all(!is.na(env)))
  # check the length is 4
  expect_length(env, 4)

})


test_that("st_envelope(): sfc_POINT xyz", {
  skip_if_not_installed("sfheaders")
  library(sfheaders)

  set.seed(0)
  m <- matrix(runif(200, -180, 180), ncol = 2)

  pnts_sfc <- sfc_point(
    cbind(m, z = runif(100))
  )

  env <- st_envelope(pnts_sfc)

  # check that its an envelopp
  expect_s3_class(env, "envelope")
  # check that all are not missing
  expect_true(all(!is.na(env)))
  # check the length is 6
  expect_length(env, 6)

})

test_that("st_envelope(): sfc_POINT xyzm", {
  skip_if_not_installed("sfheaders")
  library(sfheaders)

  set.seed(0)
  m <- matrix(runif(200, -180, 180), ncol = 2)

  pnts_sfc <- sfc_point(
    cbind(m, z = runif(100), m = runif(100))
  )

  env <- st_envelope(pnts_sfc)

  # check that its an envelopp
  expect_s3_class(env, "envelope")
  # check that all are not missing
  expect_true(all(!is.na(env)))
  # check the length is 6
  expect_length(env, 8)

})


# sfc_MULTIPOINT ----------------------------------------------------------


test_that("st_envelope(): sfc_MULTIPOINT xy", {
  skip_if_not_installed("sfheaders")

  m <- matrix(runif(200, -180, 180), ncol = 2)

  # multipoint
  mpnt_df <- as.data.frame(cbind(m, sort(rep(1:5, 100/5))))
  colnames(mpnt_df) <- c("x", "y", "multipoint_id")

  mpnt_sfc <- sfc_multipoint(mpnt_df, "x", "y", multipoint_id = "multipoint_id")

  env <- st_envelope(mpnt_sfc)

  # check that its an envelopp
  expect_s3_class(env, "envelope")
  # check that all are not missing
  expect_true(all(!is.na(env)))
  # check the length is 6
  expect_length(env, 4)

})

test_that("st_envelope(): sfc_MULTIPOINT xyz", {
  skip_if_not_installed("sfheaders")

  m <- matrix(runif(200, -180, 180), ncol = 2)

  # multipoint
  mpnt_df <- as.data.frame(cbind(m, sort(rep(1:5, 100/5))))
  colnames(mpnt_df) <- c("x", "y", "multipoint_id")

  xyzm_mpnt <- sfc_multipoint(
    cbind(mpnt_df, z = runif(100)),
    "x", "y", "z", multipoint_id = "multipoint_id"
  )

  env <- st_envelope(xyzm_mpnt)

  # check that its an envelopp
  expect_s3_class(env, "envelope")
  # check that all are not missing
  expect_true(all(!is.na(env)))
  # check the length
  expect_length(env, 6)

})


test_that("st_envelope(): sfc_MULTIPOINT xyzm", {
  skip_if_not_installed("sfheaders")

  m <- matrix(runif(200, -180, 180), ncol = 2)

  # multipoint
  mpnt_df <- as.data.frame(cbind(m, sort(rep(1:5, 100/5))))
  colnames(mpnt_df) <- c("x", "y", "multipoint_id")

  xyzm_mpnt <- sfc_multipoint(
    cbind(mpnt_df, z = runif(100), m = runif(100)),
    "x", "y", "z", "m", "multipoint_id"
  )

  env <- st_envelope(xyzm_mpnt)

  # check that its an envelopp
  expect_s3_class(env, "envelope")
  # check that all are not missing
  expect_true(all(!is.na(env)))
  # check the length
  expect_length(env, 8)

})


# sfc_POLYGON -------------------------------------------------------------

test_that("st_envelope(): sfc_POLYGON xy", {
  skip_if_not_installed("sfheaders")

  m <- matrix(c(
    0, 0,
    5, 0,
    5, 5,
    0, 5,
    0, 0
  ), ncol = 2, byrow = TRUE)

  ply <- st_sfc(st_polygon(list(m)))

  env <- st_envelope(ply)

  # check that its an envelopp
  expect_s3_class(env, "envelope")
  # check that all are not missing
  expect_true(all(!is.na(env)))
  # check the length
  expect_length(env, 4)

})


test_that("st_envelope(): sfc_POLYGON xyz", {
  skip_if_not_installed("sfheaders")

  m <- matrix(c(
    0, 0,
    5, 0,
    5, 5,
    0, 5,
    0, 0
  ), ncol = 2, byrow = TRUE)

  m <- cbind(m, runif(5))

  ply <- st_sfc(sfheaders::sfg_polygon(m, close = FALSE))

  env <- st_envelope(ply)

  # check that its an envelopp
  expect_s3_class(env, "envelope")
  # check that all are not missing
  expect_true(all(!is.na(env)))
  # check the length
  expect_length(env, 6)

})


test_that("st_envelope(): sfc_POLYGON xyzm", {
  skip_if_not_installed("sfheaders")

  m <- matrix(c(
    0, 0,
    5, 0,
    5, 5,
    0, 5,
    0, 0
  ), ncol = 2, byrow = TRUE)

  m <- cbind(m, runif(5), rnorm(5))

  ply <- st_sfc(sfheaders::sfg_polygon(m, close = FALSE))

  env <- st_envelope(ply)

  # check that its an envelopp
  expect_s3_class(env, "envelope")
  # check that all are not missing
  expect_true(all(!is.na(env)))
  # check the length
  expect_length(env, 8)

})


# sfc_LINESTRING ----------------------------------------------------------




test_that("st_envelope(): sfc_LINESTRING xy", {
  skip_if_not_installed("sfheaders")

  lns <- st_sfc(structure(
    c(7.5337216, 7.5334609, 51.9555585, 51.9557618),
    dim = c(2L, 2L),
    class = c("XY", "LINESTRING", "sfg")
  ))

  env <- st_envelope(lns)

  # check that its an envelopp
  expect_s3_class(env, "envelope")
  # check that all are not missing
  expect_true(all(!is.na(env)))
  # check the length
  expect_length(env, 4)

})


test_that("st_envelope(): sfc_LINESTRING xyz", {
  skip_if_not_installed("sfheaders")

  lns <- st_sfc(structure(
    c(7.5337216, 7.5334609, 51.9555585, 51.9557618, 999, 99),
    dim = c(2L, 3L),
    class = c("XYZ", "LINESTRING", "sfg")
  ))

  env <- st_envelope(lns)

  # check that its an envelopp
  expect_s3_class(env, "envelope")
  # check that all are not missing
  expect_true(all(!is.na(env)))
  # check the length
  expect_length(env, 6)
})


test_that("st_envelope(): sfc_LINESTRING xym", {
  skip_if_not_installed("sfheaders")

  lns <- st_sfc(structure(
    c(7.5337216, 7.5334609, 51.9555585, 51.9557618, 999, 99, -3.14, 3.14),
    dim = c(2L, 4L),
    class = c("XYZM", "LINESTRING", "sfg")
  ))

  env <- st_envelope(lns)

  # check that its an envelopp
  expect_s3_class(env, "envelope")
  # check that all are not missing
  expect_true(all(!is.na(env)))
  # check the length
  expect_length(env, 8)
})



# TODO --------------------------------------------------------------------



test_that("st_envelope(): sfc_MULTIPOLYGON", {
  skip_if_not_installed("sfheaders")
})

test_that("st_envelope(): sfc_MULTILINESTRING", {
  skip_if_not_installed("sfheaders")
})
