test_that("prepare_spatial_filter(): supports multiple types", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  # Works with bbox input
  expect_snapshot(
    prepare_spatial_filter(
      filter_geom = sf::st_bbox(nc[1, ]),
      predicate = "intersects"
    )
  )

  # Expect message for MULTIPOLYGON input
  expect_message(
    prepare_spatial_filter(
      filter_geom = sf::st_geometry(nc[1, ]),
      predicate = "intersects"
    )
  )

  # Works with length > 1 sfc input
  expect_snapshot(
    prepare_spatial_filter(
      filter_geom = sf::st_geometry(nc[1:2, ]),
      predicate = "intersects"
    )
  )
})

test_that("`prepare_spatial_filter()`: errors", {
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  expect_error(
    prepare_spatial_filter(
      filter_geom = sf::st_geometry(nc[1, ])
    )
  )
})
