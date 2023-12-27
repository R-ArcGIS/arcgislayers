test_that("multiplication works", {
  skip("Must be ran interactively")
  set_auth_token(auth_code())

  fsrv <- create_feature_server("Test Service")
  expect_snapshot(fsrv)
})
