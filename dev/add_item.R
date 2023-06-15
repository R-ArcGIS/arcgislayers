# # set_auth_token(auth_code())
# x <- sf::st_set_crs(sf::st_centroid(sfdep::guerry[5:7,3]), 27572)
# title = "tst"
#
# user = Sys.getenv("ARCGIS_USER")
# description = ""
# tags = character(0)
# snippet = ""
# categories = character(0)
# async = FALSE
# type = "Feature Service"
# host = "https://arcgis.com"
# token = Sys.getenv("ARCGIS_TOKEN")



#' Add an sf object to a Portal
#'
#'
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

  # create teh feature collection json
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

  item_id <- parsed[["id"]]
  data.frame(parsed)
}





#
# library(httr2)
#
# req <- request("https://arcgis.com/sharing/rest/content/users/jparry_ANGP/publish")
#
# req <- req_body_form(
#   req,
#   itemID = item_id,
#   fileType = "featureCollection",
#   publishParameters = '
# {"name": "table test"}',
#   f = "json",
#   token = token
# )
#
# resp <- req_perform(req)
#
# resp_body_string(resp)
#
#
#
#


