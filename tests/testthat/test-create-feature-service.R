test_that("create_feature_server() returns a FeatureServer", {
  skip("Must be ran interactively")
  set_auth_token(auth_code())

  fsrv <- create_feature_server("Test Service")

  expect_s3_class(fsrv, "FeatureServer")
  expect_identical(fsrv[["name"]], "Test Service")
})
