#' fields will always take preference over .data
add_fields <- function(feature, .data = NULL, fields = NULL, token = Sys.getenv("ARCGIS_TOKEN")) {

  if (!is.null(.data) && !is.null(fields)) {
    warning(
      "Both `.data` and `fields` were provided. Using `fields`."
    )
  }

  if (is.null(fields) && !is.null(.data)) fields <- infer_esri_type(.data)

  if (is.null(.data) && is.null(fields)) {
    stop("`.data` or `fields` must be provided")
  }

  field_json <- jsonify::to_json(
    list(fields = purrr::transpose(fields)),
    unbox = TRUE
  )

  # begin making the request
  b_url <- flayer[["url"]]

  # https://developers.arcgis.com/rest/services-reference/online/add-to-definition-feature-layer-.htm
  req <-
    httr2::request(b_url)


  reqq <- httr2::req_body_raw(
    req,
    field_json
  ) |>
    httr2::req_url_query(token = token, f = "json")


  resp <- httr2::req_perform(reqq)

  httr2::resp_body_string(resp) |>
    RcppSimdJson::fparse()

}


#' Given a data frame infer the esri field types
infer_esri_type <- function(.data) {

  if (!inherits(.data, "data.frame")) stop("`.data` must be a data frame like object")
  if (inherits(.data, "sf")) .data <- sf::st_drop_geometry(.data)

  # field mappings
  field_base_types <- vapply(.data, typeof, character(1))
  date_check <- vapply(.data, is_date, logical(1))

  field_base_types[date_check] <- "date"

  data.frame(
    name = colnames(.data),
    type = vec_mapping[field_base_types],
    alias = colnames(.data),
    nullable = TRUE,
    editable = TRUE
  )

}




#' Create a lazy frame prototype
#'
#' Given the fields of a feature layer create a lazy frame with
#' the name field names and the corresponding R type. Used for partial_eval
#'
#' @details
#'
#' - `get_ptype()` takes a scalar character containing the Esri field type and returns a prototype of the pertinent R type
#' - `remote_ptype_tbl()` takes a data frame of fields as derived from `list_fields()` and creates a lazy table
#' @keywords internal
#' @rdname field_mapping
remote_ptype_tbl <- function(fields) {

  ftype <- fields[["type"]]
  fname <- fields[["name"]]

  dbplyr::lazy_frame(as.data.frame(lapply(setNames(ftype, fname), get_ptype)))

}


#' @keywords internal
#' @rdname field_mapping
get_ptype <- function(field_type) {
  res <- switch(
    field_type,
    "esriFieldTypeSmallInteger" = integer(1),
    "esriFieldTypeSingle" = double(1),
    "esriFieldTypeGUID" = integer(1),
    "esriFieldTypeOID" = integer(1),
    "esriFieldTypeInteger" = integer(1),
    "esriFieldTypeBigInteger" = double(1),
    "esriFieldTypeDouble" = double(1),
    "esriFieldTypeString" = character(1),
    "esriFieldTypeDate" = Sys.Date()
  )

  if (is.null(res)) stop("Column of type `", field_type, "` cannot be mapped")

  res
}


vec_mapping <- c(
  "double" = "esriFieldTypeDouble",
  "integer" = "esriFieldTypeInteger",
  "character" = "esriFieldTypeString",
  # date will be manually defined as being Date or POSIX
  "date" = "esriFieldTypeDate",
  # i think....
  "raw" = "esriFieldTypeBlob"
)


# Date handling -----------------------------------------------------------

#' Date handling
#'
#' @keywords internal
#' @rdname dates
#' @details
#' - `is_date()` checks to see if an object is a date
#' - `date_to_ms()` converts a date to miliseconds from Unix Epoch
is_date <- function(x) inherits(x, c("Date", "POSIXt"))

# a function to convert dates to ms
#' @rdname dates
#' @keywords internal
date_to_ms <- function(x, tz = "UTC") {
  as.numeric(as.POSIXlt(x, tz = tz)) * 1000
}


# notes -------------------------------------------------------------------


# fields is a dateframe

# users are to provide a character vector name of the
# OID column esriFieldTypeOID
# global ID would be inferred by the feature layer or
# provided by the user I suspect esriFieldTypeGlobalID

# list columns will be omitted and a warning emitted

# field types that will be ignored
# esriFieldTypeSmallInteger
# esriFieldTypeSingle
# esriFieldTypeGeometry (not sure when this would be used)
# esriFieldTypeRaster (not sure when this would be used)
# esriFieldTypeGUID (not sure when this would be used)
# esriFieldTypeXML (oh boy i hope no one has to use this lol)
# esriFieldTypeBigInteger (not sure how this would be supported)

# by default when adding new feature only fields in the feature
# layer should be snet up because they will be ignored
# if there are non-matching field names emit a warning and
# suggest them to use update_fields
