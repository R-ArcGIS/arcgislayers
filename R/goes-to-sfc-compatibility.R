

#' Cast geos geometry to sfc object
#'
#' geos geometry cannot be used directly by sf so it must be converted. Conversion
#' using sf is slow. This function checks the types of geometries from our geojson
#' it then casts them "up" (e.g. from polygon to multipolygon) if necessary and
#' sets the propery geometry type for sf. Then casts to sf. This saves a lot of time.
#'
#' Based on https://github.com/paleolimbot/geos/issues/80.
#'
#' @param geom_types
#'
#' @keywords internal
make_sf_compat <- function(geometry) {

  all_types <- c(
    "linestring",
    "multilinestring",
    "point",
    "multipoint",
    "polygon",
    "multipolygon"
  )

  # note difference between geoms and geom
  geoms_types <- geos::geos_type(geometry)
  geom_types <- unique(geoms_types)

  type_index <- all_types %in% geom_types
  n_types <- sum(type_index)
  # if more than two types match automatically a GEOMTRY
  # if exactly one type then it is cast properly
  # both conditions don't need any manipulation
  if (n_types == 1 || n_types > 2) {
    return(sf::st_as_sfc(geometry))
  # if there is exactly two types of matches these are either multi
  # or incompatible matches
    # LINESTRING checks
  } else if (all(type_index[1:2])) {
    is_linestring <- geoms_types == all_types[1]

    geometry[is_linestring] <-
      geos::geos_make_collection(
        res_geometry[is_linestring],
        all_types[2],
        seq_len(sum(is_linestring))
      )

    return(sf::st_as_sfc(geometry))

    # POINT / MULTIPOINT check
  } else if (all(type_index[3:4])) {

    is_point <- geoms_types == all_types[3]

    geometry[is_point] <-
      geos::geos_make_collection(
        res_geometry[is_point],
        all_types[4],
        seq_len(sum(is_point))
      )

    return(sf::st_as_sfc(geometry))

    # POLYGON / MULTIPOLYGON check
  } else if (all(type_index[5:6])) {
    is_polygon <- geoms_types == all_types[5]

    geometry[is_polygon] <-
      geos::geos_make_collection(
        geometry[is_polygon],
        all_types[6],
        seq_len(sum(is_polygon))
      )

    return(sf::st_as_sfc(geometry))

  } else {
    return(sf::st_as_sfc(geometry))
  }
}

