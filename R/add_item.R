#' Publish Content
#'
#' Publishes an `sf` or `data.frame` object to an ArcGIS Portal as a
#' FeatureCollection.
#'
#' @details
#'
#'  `r lifecycle::badge("experimental")`
#'
#' - `add_item()` takes a data.frame like object and uploads it as an item in
#'   your portal.
#' - `publish_item()` takes an ID of an item in your portal and publishes it
#'   as a feature service.
#' - `publish_layer()` is a high-level wrapper that first adds an object as
#'   an item in your portal and subsequently publishes it for you.
#' - `.publish_params()` is a utility function to specify optional publish
#'   parameters such as copyright text, and the spatial reference of the
#'   published feature collection.
#'
#' Note that there is _only_ support for feature services meaning that only
#' tables and feature layers can be made by these functions.
#'
#' ### Publish Parameters
#'
#' When publishing an item to a portal, a number of [publish parameters](https://developers.arcgis.com/rest/users-groups-and-items/publish-item.htm#GUID-9E8F8526-5D58-4706-95F3-432905CC3303) can be provided. Most importantly is the `targetSR` which will be
#' the CRS of the hosted feature service. By default this is `EPSG:3857`.
#'
#' `publish_layer()` will use the CRS of the input object, `x`, by default. If
#' publishing content in two steps with `add_item()` and `publish_item()`, use
#' `.publish_params()` to craft your publish parameters. Ensure that the CRS
#' provided to `target_crs` matches that of the item you added with
#' `add_item()`.
#'
#' @inheritParams arcgisutils::as_layer
#' @param user default environment variable `Sys.getenv("ARCGIS_USER")`.
#'   The username to publish the item under.
#' @param description a length 1 character vector containing the description of
#'   the item that is being added. Note that the value cannot be larger than 64kb.
#' @param tags a character vector of tags to add to the item.
#' @param snippet a length 1 character vector with no more than 2048 characters.
#' @param categories a character vector of the categories of the item.
#' @param async default `FALSE`. Cannot be changed at this time.
#' @param type default `"Feature Service"`. Must not be changed at this time.
#' @inheritParams arcgisutils::refresh_token
#' @export
#' @rdname publish
#' @examples
#' if (interactive()) {
#'   nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#'   x <- nc[1:5, 13]
#'
#'   token <- auth_code()
#'   set_arc_token(tkn)
#'
#'   publish_res <- publish_layer(
#'     x, "North Carolina SIDS sample"
#'   )
#' }
#' @returns
#' A named list containing the url of the newly published service.
add_item <- function(
    x,
    title,
    description = "",
    tags = character(0),
    snippet = "",
    categories = character(0),
    async = FALSE,
    type = "Feature Service",
    token = arc_token()
) {


  # validate the token
  obj_check_token(token)

  # check that there is a user associated with the token
  check_token_has_user(token)

  # extract username
  user <- token[["username"]]

  # fetch the host from the token
  host <- token[["arcgis_host"]]

  # if async = TRUE stop
  # type must be feature service right now
  # TODO make this cli_abort()
  stopifnot(
    "`async` must be `FALSE`" = !async,
    "`type` must be `\"Feature Service\"`" = identical(type, "Feature Service")
  )

  # if CRS is missing require user input if interactive
  if (interactive() && is.na(sf::st_crs(x)) && inherits(x, "sf")) {
    choice <- utils::menu(
      c("Yes", "No"),
      title = "CRS is missing from `x`. Continue?"
    )

    if (choice == 2L) {
      # TODO cli_abort
      stop("Aborting. CRS is missing.")
    } else {
      # TODO cli_warn
      warning("Set the CRS to prevent this interruption.\n  - use `sf::st_set_crs()`")
    }
  } else if (!interactive() && is.na(sf::st_crs(x))) {
    # TODO cli_warn
    warning(
      "CRS is missing from `x`\nAssuming EPSG:3857."
    )
  }

  # check if snippet is too long
  # TODO cli_warn
  if (nchar(snippet) > 2048) warning("Snippet must be 2048 or fewer characters.")

  # check if description is too big or too many eles
  descrip_kb <- as.numeric(utils::object.size(description)) / 1000

  # TODO cli_abort
  stopifnot(
    "`description` must be smaller than 64kb" = descrip_kb <= 64,
    "`description` must be length 1" = length(description) == 1
  )

  req_url <- paste0(host, "/sharing/rest/content/users/", user, "/addItem")

  # create the feature collection json
  feature_collection <- as_feature_collection(
    list(as_layer(x, title, title))
  )

  # if not sf object we gotta not pass in spatial reference or extent
  if (!inherits(x, "sf")) {
    extent <- NULL
    spatial_reference <- NULL
  } else {
    extent <- paste0(sf::st_bbox(x), collapse = ",")
    spatial_reference <- jsonify::to_json(
      validate_crs(sf::st_crs(x))[[1]],
      unbox = TRUE
    )
  }

  req_fields <- compact(
    list(
      title = title,
      description = description,
      tags = tags,
      snippet = snippet,
      text = jsonify::to_json(feature_collection, unbox = TRUE),
      extent = extent,
      spatialReference = spatial_reference,
      categories = categories,
      type = "Feature Collection",
      async = async,
      url = host,
      f = "json"
    )
  )

  req <- arc_base_req(req_url, token)
  req_body <- httr2::req_body_form(req, !!!req_fields)
  resp <- httr2::req_perform(req_body)
  parsed <- RcppSimdJson::fparse(httr2::resp_body_string(resp))
  detect_errors(parsed)
  data.frame(parsed)
}



#' @export
#' @param item_id the ID of the item to be published.
#' @param file_type default `"featureCollection"`. Cannot be changed.
#' @param publish_params a list of named values of the `publishParameters`. Must match
#'   the values in the [/publish endpoint documentation](https://developers.arcgis.com/rest/users-groups-and-items/publish-item.htm#GUID-9E8F8526-5D58-4706-95F3-432905CC3303).
#' @inheritParams add_item
#' @rdname publish
publish_item <- function(
    item_id,
    publish_params = .publish_params(),
    file_type = "featureCollection",
    token = arc_token()
) {

  # validate the token
  obj_check_token(token)

  # check that there is a user associated with the token
  check_token_has_user(token)

  # extract username
  user <- token[["username"]]

  # fetch the host
  host <- token[["arcgis_host"]]

  # create request URL
  # TODO check for trailing `/` in host (should create a `sanitize_host()`)
  req_url <- paste0(host, "/sharing/rest/content/users/", user, "/publish")

  # add token and agent
  base_req <- arc_base_req(req_url, token)


  # create request
  req <- httr2::req_body_form(
    base_req,
    itemID = item_id,
    fileType = file_type,
    publishParameters = jsonify::to_json(publish_params, unbox = TRUE),
    f = "json",
  )

  resp <- httr2::req_perform(req)
  res <- RcppSimdJson::fparse(httr2::resp_body_string(resp))

  # check if any errors occurred
  detect_errors(res)

  res
}


#' @export
#' @rdname publish
#' @param ... arguments passed into `add_item()`.
publish_layer <- function(
    x,
    title,
    ...,
    publish_params = .publish_params(title, target_crs = sf::st_crs(x)),
    token = arc_token()
) {

  adtl_args <- rlang::list2(...)

  item_res <- rlang::inject(
    add_item(
      x,
      title,
      token = token,
      !!!adtl_args
    )
  )


  # fetch item_id
  item_id <- item_res[["id"]]

  published_item <- publish_item(
    item_id,
    publish_params = publish_params,
    token = token
  )

  published_item
}


#' @export
#' @rdname publish
#' @param max_record_count the maximum number of records that can be returned
#'  from the created Feature Service.
#' @param target_crs the CRS of the Feature Service to be created. By default,
#'  `EPSG:3857`.
#' @param copyright an optional character scalar containing copyright text to
#'  add to the published Feature Service.
.publish_params <- function(
    name = NULL,
    description = NULL,
    copyright = NULL,
    target_crs = 3857,
    max_record_count = 2000L
) {

  # https://developers.arcgis.com/rest/users-groups-and-items/publish-item.htm#GUID-9E8F8526-5D58-4706-95F3-432905CC3303
  # FeatureCollection publish parameters:
  # - name
  # - description
  # - maxRecordCount
  # - copyrightText
  # - layerInfo (ignore for now. No good use case)
  # - targetSR (derive from the object)

  check_null_or_scalar(name)
  check_null_or_scalar(description)
  check_null_or_scalar(copyright)

  if (is.na(target_crs)) {
    target_sr <- NULL
  } else {
    target_sr <- validate_crs(target_crs)[[1]]
  }

  compact(
    list(
      name = name,
      description = description,
      copyright = copyright,
      maxRecordCount = as.integer(max_record_count),
      targetSR = target_sr
    )
  )
}

