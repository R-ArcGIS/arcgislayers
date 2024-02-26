#' Create a FeatureServer
#'
#' Creates an empty FeatureServer with no additional layers.
#'
#' @param service_name Feature Service name.
#' @param description default blank. The description of the feature server.
#' @param crs default `3857`. A coordinate reference system to set for the feature server.
#'  Must be compatible with `sf::st_crs()`.
#' @param capabilities default full capabilities. Character vector of capabilities.
#' @param query_formats default json and geojson. May be restricted by site-wide settings.
#' @param initial_extent optional. A named list with element of
#'  `xmin`, `xmax`, `ymin`, and `ymax`. Values must be in the same CRS as `crs`.
#' @param max_record_count default `1000`. The maximum number of records that can be
#'  retrieved from a layer in one request.
#' @param allow_updates default `TRUE`. Determine if geometries can be updated.
#' @param copyright default blank. Copyright notice to provide in the Feature Server
#' @param has_static_data default `FALSE`. Indicates if data is changing.
#' @param xss_prevention cross-site-scripting prevention is enabled by default.
#'   See details for more.
#' @inheritParams add_item
#'
#' @returns
#' If a `FeatureServer` is created successfully, a `FeatureServer` object is returned
#' based on the newly created feature server's url.
#'
#' @details
#'
#' `r lifecycle::badge("experimental")`
#'
#' @export
#' @examples
#' \dontrun {
#'   set_arc_token(auth_code())
#'   create_feature_server("My empty feature server")
#' }
create_feature_server <- function(
    service_name,
    description = "",
    crs = 3857,
    capabilities = c("create", "delete", "query", "update", "editing"),
    query_formats = c("json", "geojson"),
    initial_extent = list(xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL),
    max_record_count = 1000L,
    allow_updates = TRUE,
    copyright = "",
    has_static_data = FALSE,
    xss_prevention = xss_defaults(),
    token = arc_token()
) {

  match.arg(capabilities, several.ok = TRUE)
  # TODO add common parameters (such as tag and description)
  # https://developers.arcgis.com/rest/users-groups-and-items/common-parameters.htm

  # validate the token
  obj_check_token(token)

  # check that there is a user associated with the token
  check_token_has_user(token)

  # extract username
  user <- token[["username"]]

  # fetch the host
  host <- token[["arcgis_host"]]

  # create the request url
  req_url <- paste0(host, "/sharing/rest/content/users/", user, "/createService")

  # ensure the CRS is valid
  valid_crs <- validate_crs(crs)

  # create the body of the request
  body <- compact(
    list(
      "name" = service_name,
      "serviceDescription" = description,
      "hasStaticData" = has_static_data,
      "maxRecordCount" = as.integer(max_record_count),
      "supportedQueryFormats" = paste0(tolower(query_formats), collapse = ","),
      "capabilities" =  paste0(tolower(capabilities), collapse = ","),
      "description" = "",
      "copyrightText" = "",
      "spatialReference" = valid_crs[["spatialReference"]],
      "initialExtent" = c(initial_extent, valid_crs),
      "allowGeometryUpdates" = allow_updates,
      "xssPreventionInfo" = xss_defaults()
    )
  )

  # convert to json string
  body_json <- jsonify::to_json(body, unbox = TRUE)

  # add agent and token
  req <- arc_base_req(req_url, token)

  req <- httr2::req_url_query(
    req,
    outputType = "featureService",
    f = "json"
  )

  req <- httr2::req_body_form(
    req,
    createParameters = jsonify::to_json(body, unbox = TRUE)
  )

  resp <- httr2::req_perform(req)

  resp_parsed <- RcppSimdJson::fparse(httr2::resp_body_string(resp))

  detect_errors(resp_parsed)

  arc_open(resp_parsed[["encodedServiceURL"]])

}

#' @export
#' @rdname create_feature_server
xss_defaults <- function() {
  list(
    "xssPreventionEnabled" = TRUE,
    "xssPreventionRule" = "input",
    "xssInputRule" = "rejectInvalid"
  )
}


# IDEALLY we can create a layer with the field names and types etc already specified
# then we can use `add_features()` to add data in parallel. This will likely be much
# faster than uploading a file directly

