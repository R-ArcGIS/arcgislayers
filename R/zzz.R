.onLoad <- function(...) {
  vctrs::s3_register("sf::st_as_sfc", "envelope")
  vctrs::s3_register("sf::st_crs", "FeatureLayer")
  vctrs::s3_register("sf::st_crs", "ImageServer")
  vctrs::s3_register("dplyr::collect", "FeatureLayer")
  vctrs::s3_register("dplyr::collect", "Table")
  vctrs::s3_register("dplyr::filter", "Table")
  vctrs::s3_register("dplyr::filter", "FeatureLayer")
  vctrs::s3_register("dplyr::select", "FeatureLayer")
  vctrs::s3_register("dplyr::select", "Table")
}

