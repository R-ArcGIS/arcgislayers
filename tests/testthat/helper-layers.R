expect_layer <- function(x, name, geometry_type, crs) {
  expect_s3_class(x, "FeatureLayer")
  expect_identical(x[["name"]], name)
  expect_identical(x[["geometryType"]], geometry_type)
  expect_identical(x[["extent"]][["spatialReference"]][["latestWkid"]], crs)
}
