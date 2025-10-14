test_that("query_layer_attachments() default args", {
  skip_on_cran()
  furl <- "https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/v8_Wide_Area_Search_Form_Feature_Layer___a2fe9c/FeatureServer/0"
  layer <- arc_open(furl)

  # connect to the layer
  expect_no_error(query_layer_attachments(layer))
})


test_that("query_layer_attachments() no metadata", {
  skip_on_cran()
  furl <- "https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/v8_Wide_Area_Search_Form_Feature_Layer___a2fe9c/FeatureServer/0"
  layer <- arc_open(furl)

  att <- query_layer_attachments(layer, return_metadata = FALSE)
  expect_true(all(is.na(att$exifInfo)))
})


test_that("query_layer_attachments() filter on layer field", {
  skip_on_cran()
  furl <- "https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/v8_Wide_Area_Search_Form_Feature_Layer___a2fe9c/FeatureServer/0"
  layer <- arc_open(furl)

  att <- query_layer_attachments(layer, "followup_status = 'needs_followup'")
  expect_equal(nrow(att), 24L)
})

test_that("query_layer_attachments() filter on attachment field", {
  skip_on_cran()
  furl <- "https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/v8_Wide_Area_Search_Form_Feature_Layer___a2fe9c/FeatureServer/0"
  layer <- arc_open(furl)

  att <-
    query_layer_attachments(
      layer,
      attachments_definition_expression = "att_name like '%image0%'"
    )
  expect_true(all(grepl("image0*", att$name)))
})


test_that("download_attachments()", {
  skip_on_cran()
  furl <- "https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/v8_Wide_Area_Search_Form_Feature_Layer___a2fe9c/FeatureServer/0"
  layer <- arc_open(furl)

  tmp <- tempdir()
  att <-
    query_layer_attachments(
      layer,
      attachments_definition_expression = "att_name like '%image0%'"
    )
  res <- download_attachments(att, tmp, overwrite = TRUE)
  expect_true(all(basename(unlist(res)) %in% list.files(tmp)))
})
