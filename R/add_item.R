
#' Add an object to a Portal
#'
#' Publishes an `sf` or `data.frame` object to an ArcGIS Portal.
#'
#' - `add_item()` takes a data.frame like object and uploads it as an item in your portal.
#' - `publish_item()` takes an ID of an item in your portal and publishes it as a feature service.
#' - `publish_layer()` is a high-level wrapper that first adds an object as an item
#' in your portal and subsequently publishes it for you.
#'
#' Note that there is _only_ support for feature services meaning that only tables
#' and feature layers can be made by these functions.
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
add_item <- function(
    x,
    title,
    user = Sys.getenv("ARCGIS_USER"),
    description = "",
    tags = character(0),
    snippet = "",
    categories = character(0),
    async = FALSE,
    type = "Feature Service",
    host = "https://arcgis.com",
    token = Sys.getenv("ARCGIS_TOKEN")
) {

  # if async = TRUE stop
  # type must be feature service right now
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
      stop("Aborting. CRS is missing.")
    } else {
      warning("Set the CRS to prevent this interruption.\n  - use `sf::st_set_crs()`")
    }
  } else if (!interactive() && is.na(sf::st_crs(x))) {
    warning(
      "CRS is missing from `x`\nAssuming EPSG:3857."
    )
  }

  # check if snippet is too long
  if (nchar(snippet) > 2048) warning("Snippet must be 2048 or fewer characters.")

  # check if description is too big or too many eles
  descrip_kb <- as.numeric(object.size(description)) / 1000

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
    spatial_reference <- jsonify::to_json(validate_crs(sf::st_crs(x))[[1]], unbox = TRUE)
  }

  req_fields <- compact(
    list(
      title = title,
      description = description,
      tags = tags,
      snippet = snippet,
      text = jsonify::to_json(feature_collection, unbox = TRUE),
      # text = jsn,
      extent = extent,
      spatialReference = spatial_reference,
      categories = categories,
      type = "Feature Collection",
      async = async,
      token = token,
      url = host,
      f = "json"
    )
  )


  req <- httr2::request(req_url)
  req_body <- httr2::req_body_form(req, !!!req_fields)
  resp <- httr2::req_perform(req_body)

  parsed <- RcppSimdJson::fparse(httr2::resp_body_string(resp))

  data.frame(parsed)
}



#' @export
#' @param item_id the ID of the item to be published.
#' @param file_type default `"featureCollection"`. Must not be changed at this time.
#' @param publish_params a list of named values of the `publishParameters`. Must match
#'   the values in the [/publish endpoint documentation](https://developers.arcgis.com/rest/users-groups-and-items/publish-item.htm#GUID-9E8F8526-5D58-4706-95F3-432905CC3303).
#' @inheritParams add_item
#' @rdname add_item
publish_item <- function(
    item_id,
    user = Sys.getenv("ARCGIS_USER"),
    publish_params = list(maxRecordCount = 2000),
    file_type = "featureCollection",
    host = "https://arcgis.com",
    token = Sys.getenv("ARCGIS_TOKEN")
) {

  # create request URL
  # TODO check for trailing `/` in host (shoult create a `sanitize_host()`)
  req_url <- paste0(host, "/sharing/rest/content/users/", user, "/publish")

  # create request
  req <- httr2::req_body_form(
    httr2::request(req_url),
    itemID = item_id,
    fileType = file_type,
    publishParameters = jsonify::to_json(publish_params, unbox = TRUE),
    f = "json",
    token = token
  )

  resp <- httr2::req_perform(req)
  res <- RcppSimdJson::fparse(httr2::resp_body_string(resp))

  # check if any errors occurred
  detect_errors(res)

  res
}


#' @export
#' @rdname add_item
#' @param ... arguments passed into `add_item()`.
publish_layer <- function(
    x,
    title,
    ...,
    user = Sys.getenv("ARCGIS_USER"),
    publish_params = list(maxRecordCount = 2000, name = title),
    host = "https://arcgis.com",
    token = Sys.getenv("ARCGIS_TOKEN")
) {

  adtl_args <- rlang::list2(...)

  item_res <- rlang::inject(
    add_item(
      x,
      title,
      user = user,
      host = host,
      token = token,
      !!!adtl_args
    )
  )

  detect_errors(item_res)
  # fetch item_id
  item_id <- item_res[["id"]]

  published_item <- publish_item(
    item_id,
    user = user,
    publish_params = publish_params,
    host = host,
    token = token
  )

  published_item
}



