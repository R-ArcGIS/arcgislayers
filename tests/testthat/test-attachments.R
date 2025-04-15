furl <- "https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/v8_Wide_Area_Search_Form_Feature_Layer___a2fe9c/FeatureServer/0"
layer <- arc_open(furl)

test_that("query_layer_attachments() default args", {
  # connect to the layer
  expect_no_error(query_layer_attachments(layer))
})


test_that("query_layer_attachments() no metadata", {
  att <- query_layer_attachments(layer, return_metadata = FALSE)
  expect_true(all(is.na(att$exifInfo)))
})


test_that("query_layer_attachments() filter on layer field", {
  att <- query_layer_attachments(layer, "followup_status = 'needs_followup'")
  expect_equal(nrow(att), 24L)
})

test_that("query_layer_attachments() filter on attachment field", {
  att <-
    query_layer_attachments(
      layer,
      attachments_definition_expression = "att_name like 'image0%'"
    )
  expect_true(all(grepl("image0*", att$name)))
})


test_that("download_attachments()", {
  tmp <- tempdir()
  att <-
    query_layer_attachments(
      layer,
      attachments_definition_expression = "att_name like 'image0%'"
    )
  res <- download_attachments(att, tmp, overwrite = TRUE)
  expect_true(all(basename(unlist(res)) %in% list.files(tmp)))
})
