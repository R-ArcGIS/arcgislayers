feature_layer <- function(sr) {
  structure(list(spatialReference = sr), class = c("FeatureLayer", "list"))
}

image_server <- function(sr) {
  structure(
    list(extent = list(spatialReference = sr)),
    class = c("ImageServer", "list")
  )
}

test_that("st_crs() resolves ESRI authority wkids (#291)", {
  expect_identical(
    sf::st_crs(feature_layer(list(wkid = 102003L))),
    sf::st_crs("ESRI:102003")
  )
  expect_false(is.na(sf::st_crs(feature_layer(list(wkid = 102003L)))))
})

test_that("st_crs() resolves EPSG wkids (#291)", {
  expect_identical(
    sf::st_crs(feature_layer(list(wkid = 4326L))),
    sf::st_crs(4326)
  )
})

test_that("st_crs() reads an ImageServer extent (#291)", {
  expect_identical(
    sf::st_crs(image_server(list(wkid = 102003L))),
    sf::st_crs("ESRI:102003")
  )
})

test_that("st_crs() returns NA for a missing spatial reference (#291)", {
  expect_true(is.na(sf::st_crs(feature_layer(NULL))))
})
