test_that("use raster functions", {
  skip_on_cran()

  furl <- "https://di-usfsdata.img.arcgis.com/arcgis/rest/services/FIA_BIGMAP_2018_Species_Aboveground_Biomass/ImageServer"

  x <- arc_open(furl)
  expect_no_error({
    suppressWarnings({
      balsams <- arc_raster(
        x,
        xmin = -71,
        xmax = -67,
        ymin = 43,
        ymax = 47.5,
        bbox_crs = 4326,
        width = 100,
        height = 100,
        raster_fn = "SPCD_0012_Abies_balsamea"
      )
    })
  })
})

test_that("list service raster functions", {
  skip_on_cran()

  furl <- "https://di-usfsdata.img.arcgis.com/arcgis/rest/services/FIA_BIGMAP_2018_Tree_Species_Aboveground_Biomass/ImageServer"

  x <- arc_open(furl)
  raster_fns <- list_raster_fns(x)
  expect_s3_class(raster_fns, "data.frame")
  expect_s3_class(raster_fns, "tbl")
})
