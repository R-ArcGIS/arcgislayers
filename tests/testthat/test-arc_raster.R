test_that("arc_raster() works", {
  img_url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"

  landsat <- arc_open(img_url, token = "")

  bbox <- sf::st_bbox(c(xmin = -71, ymin = 43, xmax = -67, ymax = 47.5), crs = 4326)

  res <- arc_raster(landsat, bbox, 1000, 1000)

  expect_s4_class(res, "SpatRaster")
  expect_equal(attr(class(res), "package"), "terra")
})
