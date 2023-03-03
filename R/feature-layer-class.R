


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
#' @export
#' @examples
#' library(arcgis)
#' furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"
#' county_fl <- feature_layer(furl)
feature_layer <- function(url, token = Sys.getenv("ARCGIS_TOKEN")) {
  req <- httr2::request(url)
  meta <- fetch_layer_metadata(req, token = token)
  n_feats <- count_features(req, token)
  meta <- compact(meta)
  meta[["url"]] <- url

  if (meta[["type"]] != "Feature Layer") {
    stop(
      "url is not a Feature Layer",
      glue::glue(": `{meta[['type']]}` provided"),
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


# what information should be stored in the feature layer class?
# the metadata will all be stored in the metadata field
# url
# geometry type
# CRS
# number of features
# at bottom
# <n features, j fields>


# st_crs method for feature layer
st_crs.FeatureLayer <- function(obj, ...) {
  sf::st_crs(obj[["extent"]][["spatialReference"]][["latestWkid"]])
}


# Print -------------------------------------------------------------------



#' Print method for feature layer objects
#' @export
print.FeatureLayer <- function(x, ...) {

  to_print <- compact(list(
    "Name" = x[["name"]],
    "Geometry Type" = x[["geometryType"]],
    "CRS" = x[["extent"]][["spatialReference"]][["latestWkid"]],
    #"Query Formats" = x[["supportedQueryFormats"]],
    "Capabilities" = x[["capabilities"]]
  ))

  header <- sprintf(
    "<%s <%i features, %i fields>>",
    class(x), attr(x, "n"), length(x$fields$name)
    )

  body <- paste0(names(to_print), ": ", to_print)

  # cat out
  cat(header, body, sep = "\n")

  query <- compact(attr(x, "query"))

  if (any(lengths(query) > 0)) {
    # print if selection is made
    q_names <- names(query)

    q_str <- vapply(q_names, prettify_param, character(1), query, USE.NAMES = TRUE)
    q_body <- paste0(names(q_str), ": ", q_str)

    cat("Query:", q_body, sep = "\n  ")
  }



  invisible(x)
}


#' function to make printing easier
#'
#' The function takes a parameter name and the query list and trims each element
#' to fit on the contents of the window.
#'
#' @keywords internal
prettify_param <- function(param, query) {
  n_pad <- nchar(param) + 3
  cwidth <- options("width")[["width"]]
  width <- ifelse(is.null(cwidth), 20, cwidth)
  strtrim(as.character(query[[param]]), width - n_pad)
}

