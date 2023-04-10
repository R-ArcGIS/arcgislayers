parse_esri_json <- function(string) {

  # parse the string
  # ensure any json nulls are NAs
  b_parsed <- RcppSimdJson::fparse(
    string,
    empty_object = NA,
    empty_array = NA,
    single_null = NA
  )

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

  obj_classes <- c("XY", sfg_class, "sfg")

  # manually apply the sfg class
  for (i in seq_along(geo_raw)) {
    class(geo_raw[[i]]) <- obj_classes
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
