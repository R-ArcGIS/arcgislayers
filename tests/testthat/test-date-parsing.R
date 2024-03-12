test_that("date parsing works", {
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/MTBS_Polygons_v1/FeatureServer/0"
  res <- arc_select(arc_open(furl), n_max = 5, fields = "StartDate")
  expect_s3_class(res$StartDate, c("POSIXct", "POSIXt"))
})
