# Utility Functions -------------------------------------------------------



#' Retrieve metadata for a feature layer
#'
#' @param request an httr2 request object. Should be the `base_req`
#'   object that is created from the provided feature layer url
#'
#' @keywords internal
fetch_layer_metadata <- function(request, token) {
  httr2::req_url_query(
    request,
    f = "pjson",
    # TODO fill this in with authenticated token in the future
    token = token
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse(
      int64_policy = "double"
    )
}


#' Count the number of features in a feature layer
#'
#' @param request the base request created from the feature layer url.
#' @keywords internal
#' @returns a numeric with the total number of features in the feature layer
count_features <- function(request, token) {
  request |>
    httr2::req_url_path_append("query") |>
    httr2::req_url_query(
      returnCountOnly = "true",
      where = "1 = 1",
      f = "pjson",
      token = token
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse(query = "/count")
}


#' Create paginated requests
#'
#' For a feature layer, create a list of requests that use the
#' appropriate offsets. This allows us to not paginate but rather
#' create the requests in parallel with `httr2::multi_req_perform`.
#'
#' @param request the base request created from only the feature layer
#'   url.
#' @param offsets a vector of offsets. Used to create multiple requests
#'   for each requisite page.
#' @keywords internal
# TODO update to handle other query parameters and tokens
make_offset_reqs <- function(request, offsets) {
  lapply(offsets, function(offset) {
    httr2::req_url_query(
      httr2::req_url_path_append(request, "query"),
      where = "1 = 1",
      outFields = "*",
      offset = offset,
      f = "pgeojson",
      token = ""
    )
  })

}
