

#' Read geojson string response
#'
#' `collect()` performs multiple queries at a time. Each query is parsed using this function.
#' This function first reads the geojson for the fields. If there is no geometry present
#' it skips reading with geos and returns a data frame. If geometry is present, it reads
#' it with geos
#'
#' @keywords internal
read_fl_page <- function(geojson, crs) {

  feats_raw <- RcppSimdJson::fparse(
    geojson,
    empty_array = NA,
    single_null = NA,
    max_simplify_lvl = 0L
  )

  if (is.null(dim(feats_raw[["features"]]))) return(feats_raw[["properties"]])

  res_fields <- do.call(rbind.data.frame, feats_raw[["features"]][["properties"]])

  # early return if geometry is missing skips parsing with geos
  if (all(is.na(feats_raw$features$geometry))) {
    return(tibble::as_tibble(res_fields))
  }

  geometry <- geos::geos_read_geojson(geojson, crs = crs) |>
    geos::geos_unnest()

  geometry <- make_sf_compat(geometry)

  # if "geometry" is an existent column name rename to ".geometry"
  geo_taken <- "geometry" %in% names(res_fields)

  geo_name <- switch(as.character(geo_taken), "TRUE" = ".geometry", "FALSE" = "geometry")

  res_fields[[geo_name]] <- geometry

  sf::st_sf(
    tibble::as_tibble(res_fields)
  )
}


