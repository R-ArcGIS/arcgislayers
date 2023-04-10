
# Table -------------------------------------------------------------------

# url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/List_of_Providers/FeatureServer/27"
#

#' Feature Table
#'
#'
#' @examples
#' tbl_url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/List_of_Providers/FeatureServer/27"
#' feature_table(tbl_url)
#' @keywords internal
feature_table <- function(url, token = "") {

  req <- httr2::request(url)
  meta <- fetch_layer_metadata(req, token = token)
  n_feats <- count_features(req, token)
  meta <- compact(meta)
  meta[["url"]] <- url

  if (meta[["type"]] != "Table") {
    stop("url is not a Table\n", meta[['type']], " provided")
  }


  structure(
    meta,
    class = "Table",
    n = n_feats,
    query = list()
  )
}


# Feature Layer -----------------------------------------------------------

# TODO - implement:
#> select (for outFields)
#> st_relate()
#> optionally return geometry or not
#>  - if no, don't use geos
#> - group_by()
#>  - groupByFieldsForStatistic
#>  - this is only useful is summarize can be implemented
#> arrange() - orderByFields() (is this useful)
#> - basic functionality is to read with specified columns and number of rows
#>


#' Create a Feature Layer Object
#'
#' Ideally, we can use this s3 object to build up queries
#'
#' @param url the url of a feature layer
#' @param token authorization token as provided by `auth_client()` or `auth_code()`
#'
#' @examples
#' library(arcgis)
#' furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"
#' county_fl <- feature_layer(furl)
#' @keywords internal
feature_layer <- function(url, token = Sys.getenv("ARCGIS_TOKEN")) {
  req <- httr2::request(url)
  meta <- fetch_layer_metadata(req, token = token)
  n_feats <- count_features(req, token)
  meta <- compact(meta)
  meta[["url"]] <- url

  if (meta[["type"]] != "Feature Layer") {
    stop(
      "url is not a Feature Layer\n", meta[['type']], " provided",
      call. = FALSE
    )
  }

  structure(
    meta,
    class = "FeatureLayer",
    n = n_feats,
    query = list()
  )
}


# Image server ------------------------------------------------------------


# # https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer
#
# url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"
#
#
#' Image Server Representation
#' @keywords internal
image_server <- function(url, token = Sys.getenv("ARCGIS_TOKEN")) {
  req <- httr2::request(url)
  meta <- fetch_layer_metadata(req, token = token)
  meta <- compact(meta)
  meta[["url"]] <- url

  structure(
    meta,
    class = "ImageServer"
  )
}


# Feature Server ----------------------------------------------------------

# TODO implement head method
# url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/hexagons_state/FeatureServer"
# fl <- feature_layer(file.path(url, 0))
# fl


#' Create a feature server
#'
#' @param url the url of a feature server
#' @examples
#' url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/hexagons_state/FeatureServer"
#' feature_server(url)
#'@keywords internal
feature_server <- function(url, token = Sys.getenv("ARCGIS_TOKEN")) {
  req <- httr2::request(url)
  meta <- fetch_layer_metadata(req, token = token)
  meta <- compact(meta)
  meta[["url"]] <- url
  structure(
    meta,
    class = "FeatureServer"
  )
}

