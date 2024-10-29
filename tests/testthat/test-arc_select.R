test_that("arc_select(): polygons can be parsed", {

  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"
  flayer <- arc_open(furl)

  expect_no_error(arc_select(flayer))

})


test_that("arc_select(): tables can be parsed", {

  furl <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/List_of_Providers/FeatureServer/27"

  tblayer <- arc_open(furl)
  expect_no_error(arc_select(tblayer))
})


test_that("arc_select() works on `ImageServer`s", {
  img_url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"

  landsat <- arc_open(img_url)

  tmp <- arc_select(landsat, n_max = 2, where = "Month = 2")
  expect_snapshot(tmp)
})


test_that("arc_select(): respects `n_max`", {
  furl <- "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/PLACES_LocalData_for_BetterHealth/FeatureServer/0"

  flayer <- arc_open(furl)

  res <- arc_select(flayer, n_max = 999)

  expect_identical(nrow(res), 999L)
})

test_that("arc_select(): respects `n_max` & `page_size`", {
  furl <- "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/PLACES_LocalData_for_BetterHealth/FeatureServer/0"

  flayer <- arc_open(furl)

  res <- arc_select(flayer, n_max = 333, page_size = 111)

  expect_identical(nrow(res), 333L)
})


test_that("arc_select(): respects `...`", {

  furl <- "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/PLACES_LocalData_for_BetterHealth/FeatureServer/0"

  flayer <- arc_open(furl)
  # we expect an error when returnCountOnly is true
  expect_error(
    arc_select(
      flayer,
      where =  "TotalPopulation > 25000",
      fields = c("StateAbbr", "StateName"),
      returnCountOnly = "true"
    )
  )
})

test_that("arc_select(): reports informative errors with `...`", {
  furl <- paste0(
    "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest",
    "/services/PLACES_LocalData_for_BetterHealth/FeatureServer/0"
  )
  flayer <- arc_open(furl)
  expr <- arc_select(
    flayer,
    wher = FALSE,
    n_max = 1
  ) |> rlang::expr()
  
  expect_error(
    rlang::eval_bare(data), 
    "`wher` must be a single string, not `FALSE`"
  )
})
