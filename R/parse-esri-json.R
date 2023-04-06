furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

url <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0/query?returnGeometry=TRUE&outSR=%7B%22wkid%22%3A4326%7D&token=&outFields=%2A&where=1%3D1&f=json&resultOffset=2001"

body_str <- httr2::request(url) |>
  httr2::req_perform() |>
  httr2::resp_body_string()



parse_esri_json <- function(string) {

  # parse the string
  b_parsed <- RcppSimdJson::fparse(string)

  # extract the geometry features
  fts_raw <- b_parsed[["features"]]

  # bind all of them together into a single data frame
  fields <- do.call(rbind.data.frame, fts_raw[["attributes"]])

  # early return if geometry is missing skips parsing
  if (is.null(b_parsed$features$geometry)) {
    return(fields)
  }

  # extract geometry
  # this allows us to modify in place via for loop
  geo_raw <- fts_raw[["geometry"]]

  # identify what it's first element's first element's class is.
  # If it is a list then its a MULTI variant otherwise
  # its a singular linestring or polygon
  # this value is passed to `identify_class`
  list_ele_class <- class(geo_raw[[1]][[1]])

  sfg_class <- switch(
    b_parsed[["geometryType"]],
    esriGeometryPoint = "POINT",
    esriGeometryMultipoint = "MULTIPOINT",
    esriGeometryPolyline = identify_class("LINESTRING", list_ele_class),
    esriGeometryPolygon = identify_class("POLYGON", list_ele_class)
  )

  # manually apply the sfg class
  for (i in seq_along(fts_raw[["geometry"]])) {
    class(geo_raw[[i]]) <- c("XY", sfg_class, "sfg")
  }

  crs_raw <- b_parsed[["spatialReference"]][["latestWkid"]]

  sf::st_sf(
    fields, geometry = sf::st_sfc(geo_raw, crs = crs_raw)
  )
}


# identifies if an esriGeometryPolyline or esriGeometryPolygon
# is a MULTILINESTRING or MULTIPOLYGON
# requires
identify_class <- function(object_type, inner_class) {
  object_type <- toupper(object_type)
  match.arg(object_type, c("POLYGON", "LINESTRING"))
  switch(
    inner_class,
    list = paste0("MULTI", object_type),
    object_type
  )
}





geojson_url <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0/query?returnGeometry=TRUE&outSR=%7B%22wkid%22%3A4326%7D&token=&outFields=%2A&where=1%3D1&f=geojson&resultOffset=2001"

geojson_str <- httr2::request(geojson_url) |>
  httr2::req_perform() |>
  httr2::resp_body_string()



bm <- bench::mark(
  RcppSimdJson = parse_esri_json(body_str),
  geos = read_fl_page(geojson_str, 4326),
  iterations = 50,
  check = F
)

