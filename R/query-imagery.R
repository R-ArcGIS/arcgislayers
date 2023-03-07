#' Retrieve Imagery
#' @export
query_imagery <- function(
    x,
    bbox,
    format = "tiff",
    width = NULL,
    height = NULL,
    token = Sys.getenv("ARCGIS_TOKEN")
) {

  req <- httr2::request(paste0(x[["url"]], "/exportImage"))

  req <- httr2::req_url_query(
    req,
    bbox = paste0(bbox, collapse = ","),
    bboxSR = validate_crs(sf::st_crs(bbox))[["spatialReference"]][["wkid"]],
    format = format,
    size = paste0(c(width, height), collapse = ","),
    token = token,
    f = "json"
  )

  resp <- httr2::req_perform(req)

  resp_meta <- jsonify::from_json(httr2::resp_body_string(resp))


  res <- terra::rast(
    resp_meta$href
  )
  names(res) <- x[["bandNames"]]
  res
}
