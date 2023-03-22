token <- auth_client()

set_auth_token(token)

furl <- "https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/geomtesting/FeatureServer/0"

x <- feature_layer(furl)

# delete_features(x, where = "objectid > 0")

.data <- sfdep::guerry |>
  sf::st_set_crs(27572) |>
  sf::st_transform(sf::st_crs(x))

# add data
add_features(x, .data)


# bring into memory
y <- collect_layer(x)

# convert geometries to their bounding rectangles
.data <- y |>
  tibble::as_tibble() |>
  dplyr::transmute(
    OBJECTID = OBJECTID,
    geometry = sf::st_as_sfc(
      rsgeo::bounding_rectangles(rsgeo::as_rsgeom(geometry))
    )
  ) |>
  sf::st_as_sf(crs = sf::st_crs(x))

plot(.data$geometry)


# update features
update_features(x, .data)

