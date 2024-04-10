# Tests to ensure that the correct fields are return

furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Major_Cities_/FeatureServer/0"

flayer <- arc_open(furl)
# https://github.com/R-ArcGIS/arcgislayers/pull/179
test_that("arc_select(x, fields = \"\"): returns no fields", {
  res <- arc_select(flayer, fields = "")
  expect_identical(colnames(res), "geometry")
})

test_that('arc_select(x, fields = "", geometry = NULL): returns 0 columns all rows', {
  expect_identical(
    dim(arc_select(flayer, fields = "", geometry = FALSE)),
    c(4186L, 0L)
  )
})


test_that('arc_select(flayer, fields = "state_abbr") does not include OID', {
  res <- arc_select(flayer, fields = "state_abbr", n_max = 10)
  expect_identical(
    colnames(res),
    c("STATE_ABBR", "geometry")
  )
})

test_that("arc_select() doesnt remove OID with fields", {
  res <- arc_select(
    flayer,
    fields = c("state_abbr", "objectid"), n_max = 10
  )
  expect_identical(
    colnames(res),
    c("STATE_ABBR", "OBJECTID", "geometry")
  )
})

test_that("arc_select() with fields works on tables", {
  furl <- paste0(
    "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/",
    "USA_Wetlands/FeatureServer/1"
  )

  flayer <- arc_open(furl)
  expect_no_error(arc_select(flayer, fields = "", n_max = 100))

  res <- arc_select(flayer, fields = "objectid", n_max = 100)
  expect_identical(colnames(res), "OBJECTID")


  res <- arc_select(flayer, n_max = 100)
  expect_identical(
    colnames(res),
    list_fields(flayer)[["name"]]
  )
})

test_that("arc_select() works with ImageServers", {
  landsat <- arc_open(
    "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"
  )


  res <- arc_select(landsat, fields = "Name", n_max = 10)

  expect_identical(
    colnames(res),
    c("Name", "geometry")
  )

  expect_identical(
    colnames(arc_select(landsat, fields = "objectid", n_max = 1)),
    c("OBJECTID", "geometry")
  )

  expect_identical(
    colnames(
      arc_select(
        landsat,
        fields = c("name", "objectid"),
        n_max = 1,
        geometry = FALSE
      )
    ),
    c("Name", "OBJECTID")
  )
})
