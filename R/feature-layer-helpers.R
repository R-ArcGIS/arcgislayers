#' List fields in a a feature layer
#' @export
list_fields <- function(x) {
  tibble::as_tibble(x[["fields"]])
}


#' Fetch n rows from a feature layer
#' @method head FeatureLayer
#' @export
head.FeatureLayer <- function(x, n = 6, token = "", ...) {
  query_params <- validate_params(attr(x, "query"), token = token)

  # if n is too large warn
  if (n > attr(x, "n")) {
    warning(
      "`n` is larger than `maxRecordCount`",
        "\n - returning ", x[['maxRecordCount']], " rows"
      )
  }

  req <- httr2::request(x[["url"]])
  req <- httr2::req_url_query(
    httr2::req_url_path_append(req, "query"),
    !!!query_params,
    resultRecordCount = n
  )

  resp <- httr2::req_perform(req)

  read_fl_page(
    httr2::resp_body_string(resp),
    crs = x[["extent"]][["spatialReference"]][["latestWkid"]]
  )

}


#' Cleary all query parameters
#'
#' @export
clear_query <- function(x) {
  attr(x, "query") <- list()
  x
}
