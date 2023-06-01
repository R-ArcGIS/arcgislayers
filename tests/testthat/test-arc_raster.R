test_that("arc_raster() works", {
  img_url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"

  landsat <- arc_open(img_url, token = "")

  res <- arc_raster(
    landsat,
    xmin = -71,
    ymin = 43,
    xmax = -67,
    ymax = 47.5,
    crs = 4326,
    height = 100,
    width = 100
  )

  expect_s4_class(res, "SpatRaster")
  expect_equal(attr(class(res), "package"), "terra")
})
