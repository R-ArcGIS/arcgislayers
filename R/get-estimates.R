#' Get Estimates
#'
#' @inheritParams arc_select
#'
#' @references [ArcGIS REST Doc](https://developers.arcgis.com/rest/services-reference/enterprise/get-estimates-feature-service-layer-.htm)
#' @examples
#' furl <- paste0(
#'   "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/",
#'   "USA_Counties_Generalized_Boundaries/FeatureServer/0"
#' )
#'
#' county_fl <- arc_open(furl)
#' get_layer_estimates(county_fl)
#' @export
#' @returns
#' A named list containing all estimate info. If `extent` is present,
#' it is available as an object of class `bbox`.
get_layer_estimates <- function(x, token = Sys.getenv("ARCGIS_TOKEN")) {

  # check if its a supported layer
  obj_check_layer(x)

  # check if `infoInEstimates` is null
  if (is.null(x[["infoInEstimates"]])) {
    cli::cli_abort(
      "{.var {rlang::caller_arg(x)}} does not have estimates."
    )
  }

  b_req <- httr2::request(x[["url"]])


  est_req <- httr2::req_url_path_append(
    b_req,
    "getEstimates"
  )

  # token bug :[
  if (token != "") {
    resp <-
      httr2::req_url_query(est_req, f = "json") |>
      httr2::req_auth_bearer_token(token) |>
      httr2::req_perform()
  } else {
    resp <-
      httr2::req_url_query(est_req, f = "json") |>
      httr2::req_perform()
  }

  # process json string
  res_raw <- httr2::resp_body_string(resp) |>
    RcppSimdJson::fparse()

  # process extent if present
  ext <- res_raw[["extent"]]

  if (!is.null(ext)) {
    crs <- sf::st_crs(ext[["spatialReference"]][[1]])
    bbox <- sf::st_bbox(
      unlist(ext[c("xmin", "ymin", "xmax", "ymax")]),
      crs = crs
    )
    res_raw[["extent"]] <- bbox
  }

  # return the rest
  res_raw
}



