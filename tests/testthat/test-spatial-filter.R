test_that("multiplication works", {
  skip("not done developing")
  #' We want to do 1 thing for each function. Functions should be about creating
  #' list objects that turn into JSON. These objects are integrated into the requests
  #'
  #' prepare_spatial_filter() creates a list object that turns into the appropriate JSON
  #'  it works with only a single geometry object. Doing CRS matching and checking
  #'  should be done _outside_ of the function. Incorporating it into the function is
  #'  doing too much.
  #'
  # define the feature layer url
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"
  devtools::load_all()
  x <- arc_open(furl)

  # no args
  expect_snapshot(arc_select(x))

  # has field
  expect_snapshot(
    arc_select(
      x,
      fields = c("STATE_NAME", "FIPS")
    )
  )

  # has where
  expect_snapshot(
    arc_select(
      x,
      where = "STATE_NAME = 'California'"
    )
  )

  # has field & where
  ca <- arc_select(
    x,
    fields = c("STATE_NAME", "FIPS"),
    where = "STATE_NAME = 'California'"
  )

  expect_snapshot(ca)

  # filtered geometry
  # sfc

  sfc <- ca$geometry
  arc_select(x, filter_geom = sfc, predicate = "intersects")

  # bbox
  bb <- sf::st_bbox(sfc)
  arc_select(x, filter_geom = bb, predicate = "intersects")

  # sfg
  mp <- sf::st_union(sfc[1:5])
  arc_select(x, filter_geom = mp, predicate = "intersects")
  arc_select(x, filter_geom = bb, predicate = "intersects")
  # multipolygon
  #

})
