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

test_that("arc_select(): supports multiple filter_geom input types", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package="sf"))

  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_State_Boundaries/FeatureServer/0"

  flayer <- arc_open(furl)

  # allow bbox input for filter_geom
  bbox_res <- arc_select(
    flayer,
    filter_geom = sf::st_bbox(nc),
    fields = "STATE_NAME"
  )

  expect_identical(
    bbox_res[["STATE_NAME"]],
    c("Georgia", "Kentucky", "North Carolina", "South Carolina",
      "Tennessee", "Virginia")
  )

  # allow sfc input for filter_geom
  sfc_res <- suppressWarnings(
    arc_select(
      flayer,
      filter_geom = nc$geometry,
      fields = "STATE_NAME"
    )
  )

  expect_identical(
    sfc_res[["STATE_NAME"]],
    c("North Carolina", "Virginia")
  )

  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties/FeatureServer/0"

  flayer <- arc_open(furl)

  # allow sfg input for filter_geom
  sfg_res <- arc_select(
    flayer,
    filter_geom = nc$geometry[1],
    fields = "STATE_NAME"
  )

  expect_identical(
    unique(sfg_res[["STATE_NAME"]]),
    c("North Carolina", "Tennessee", "Virginia")
  )

  # allow multiple POINTs as input for filter_geom
  points_res <- arc_select(
    flayer,
    filter_geom = sf::st_sample(nc, size = 10),
    fields = "STATE_NAME"
  )

  expect_identical(
    unique(points_res[["STATE_NAME"]]),
    "North Carolina"
  )
})

test_that("arc_select(): warns for Table layers and provides message for MULTIPOLYGON input", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package="sf"))

  turl <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/List_of_Providers/FeatureServer/27"

  tlayer <- arc_open(turl)

  # warn on table URLs
  expect_warning(
    arc_select(
      tlayer,
      filter_geom = nc$geometry
    )
  )
})


test_that("arc_select(): errors for invalid filter_geom inputs", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package="sf"))

  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties/FeatureServer/0"

  flayer <- arc_open(furl)

  # error on sf input
  expect_error(
    arc_select(
      flayer,
      filter_geom = nc
    )
  )
})
