test_that("arc_read(): FeatureServer can be read", {
  skip_on_cran()
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

  layer <- arc_read(furl, n_max = 100)

  # if any errors occur above here the test will fail
  expect_true(TRUE)
})


test_that("arc_read(): ImageServer can be read", {
  skip_on_cran()
  img_url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"

  res <- arc_read(
    img_url,
    xmin = -71,
    ymin = 43,
    xmax = -67,
    ymax = 47.5,
    crs = 4326,
    height = 50,
    width = 50
  )

  expect_s4_class(res, "SpatRaster")
  expect_equal(attr(class(res), "package"), "terra")
})


test_that("arc_read(): name_repair works", {
  skip_on_cran()
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

  col_select <- c("NAME", "FIPS")

  layer <- arc_read(furl, col_select = col_select, name_repair = tolower)

  expect_named(layer, c("name", "fips", "geometry"))

  layer <- arc_read(furl, col_select = col_select, col_names = c("Name", "FIPS Code"))

  expect_named(layer, c("Name", "FIPS Code", "geometry"))

  expect_error(
    arc_read(furl, col_select = col_select, col_names = c("Name", "Name"), name_repair = "check_unique")
  )
})

test_that("arc_read(): n_max is correct", {
  skip_on_cran()
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

  expect_equal(nrow(arc_read(furl, n_max = 1)), 1L)
  expect_equal(nrow(arc_read(furl, n_max = 1234)), 1234L)
})

test_that("arc_read(): n_max option is respected", {
  skip_on_cran()
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

  # set n_max via options
  options("arcgislayers.n_max" = 1234)

  layer <- arc_read(furl)
  expect_equal(nrow(layer), 1234L)
})

test_that("arc_read(): n_max option is ignored when n_max is set", {
  skip_on_cran()
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

  # set n_max via options
  options("arcgislayers.n_max" = 1234)

  layer <- arc_read(furl, n_max = 321)
  expect_equal(nrow(layer), 321L)
})

test_that("arc_read(): correct error with unsupported type", {
  skip_on_cran()
  furl <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer"
  expect_error(arc_read(furl), "is not a supported type")
})

test_that("arc_read(): no error on tricky polylines", {
  skip_on_cran()
  url <- "https://gisportalp.itd.idaho.gov/xserver/rest/services/RH_GeneralService/MapServer/1"

  res <- arc_read(url, where = "OBJECTID = 440013")
  expect_equal(res, structure(data.frame(), null_elements = integer()))
})

test_that("arc_read(): error with invalid col_names", {
  skip_on_cran()
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"
  expect_error(arc_read(furl, n_max = 1, col_names = 1), "or a character vector")
  expect_error(arc_read(furl, n_max = 1, col_names = character(0)), "must be length 13 or shorter, not 0")
})

test_that("arc_read(): work with col_names = FALSE", {
  skip_on_cran()
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

  expect_named(arc_read(furl, fields = "", col_names = FALSE), "X1")
})

test_that("arc_read(): work with col_names vector", {
  skip_on_cran()
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

  expect_named(arc_read(furl, fields = c("NAME", "STATE_NAME"), col_names = "name"), c("name", "X2", "geometry"))
  expect_named(arc_read(furl, fields = "", col_names = "geom"), "geom")
})


test_that("arc_read(): error with invalid alias", {
  skip_on_cran()
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"
  expect_error(arc_read(furl, n_max = 1, alias = "droop"), 'Did you mean "drop"?')
})

test_that("arc_read(): work with alias replace", {
  skip_on_cran()
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"
  expect_named(arc_read(furl, n_max = 1, fields = "STATE_ABBR", alias = "replace"), c("State Abbreviation", "geometry"))
})

test_that("arc_read(): work with alias label", {
  skip_on_cran()
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

  layer <- arc_read(furl, n_max = 1, fields = "STATE_ABBR", alias = "label")
  expect_identical(attr(layer[[1]], "label"), "State Abbreviation")
})

test_that("arc_read() permits alias = \"drop\"", {
  skip_on_cran()
  furl <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3"
  expect_no_error(arc_read(furl, n_max = 20, alias = "drop"))
})

test_that("arc_read() permits alias = \"drop\"", {
  skip_on_cran()
  furl <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3"
  expect_no_error(arc_read(furl, n_max = 20, alias = "drop"))
})

test_that("arc_read() validates alias", {
  skip_on_cran()
  furl <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3"
  expect_no_error(arc_read(furl, n_max = 5, alias = NULL))
  expect_error(arc_read(furl, n_max = 5, alias = c("drop", "label")), "`alias`")
})

test_that("arc_read() permits col_names = TRUE", {
  skip_on_cran()
  furl <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3"
  expect_no_error(arc_read(furl, n_max = 5, col_names = TRUE))
})

test_that("arc_read() permits col_names = FALSE", {
  skip_on_cran()
  furl <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3"
  res <- arc_read(furl, n_max = 5, col_names = FALSE)
  expect_identical(paste("X", 1:52, sep = ""), colnames(res))
})

test_that("arc_read() throws deprecation warning", {
  skip_on_cran()
  furl <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3"
  # this should give a deprecation warning
  expect_warning(arc_read(furl, n_max = 5, col_names = "alias"))
})

test_that("set_layer_aliases() replaces names with alias", {
  skip_on_cran()
  furl <- paste0(
    "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/",
    "rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"
  )

  flayer <- arc_open(furl)
  res <- arc_select(flayer, n_max = 1)
  with_aliases <- set_layer_aliases(res, flayer)

  expect_identical(
    colnames(with_aliases),
    c(
      "OBJECTID", "Name", "State Name", "State FIPS", "FIPS", "Area in square miles",
      "2020 Total Population", "People per square mile", "State Abbreviation",
      "County FIPS", "Shape__Area", "Shape__Length", "geometry"
    )
  )
})

test_that("set_layer_aliases() puts alias as label attribute for the column", {
  skip_on_cran()
  furl <- paste0(
    "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/",
    "rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"
  )

  flayer <- arc_open(furl)
  res <- arc_select(flayer, n_max = 1)
  with_aliases <- set_layer_aliases(res, flayer, alias = "label")

  aliases <- vapply(
    sf::st_drop_geometry(with_aliases), 
    attr, 
    character(1), 
    "label", 
    USE.NAMES = FALSE
  )

  expect_identical(
    aliases,
    c(
      "OBJECTID", "Name", "State Name", "State FIPS", "FIPS", "Area in square miles",
      "2020 Total Population", "People per square mile", "State Abbreviation",
      "County FIPS", "Shape__Area", "Shape__Length", "geometry"
    )
  )
})
