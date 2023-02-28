#' Retrieve a feature layer
#'
#' Give a `FeatureLayer` or `Table` object, retrieve its data as an `sf` object or `tibble` resepctively.
#'
#' @param x an object of class `FeatureLayer` or `Table`.
#' @param fields a character vector of the field names that you wish to be returned. By default all fields are returned.
#' @param crs the spatial reference to be returned. If the CRS is different than the `FeatureLayer`'s CRS, a transformation will occur server-side. Ignored for `Table` objects.
#' @param n_max the maximum number of features to return. By default returns every feature available.
#' @param ... additional query parameters passed to `update_params()`. See [reference documentation](https://developers.arcgis.com/rest/services-reference/enterprise/query-feature-service-layer-.htm#GUID-BC2AD141-3386-49FB-AA09-FF341145F614) for possible arguments.
#' @export
get_layer <- function(x, fields, where, crs = st_crs(x), n_max = Inf, ...) {

  # handle fields and where clause if missing
  if (missing(fields)) fields <- "*"
  if (length(fields) > 1) fields <- paste0(fields, collapse = ",")
  if (missing(where)) where <- "1=1"

  # handle crs
  crs <- if (is.na(crs[["wkt"]])) {
    crs <-  NULL
  } else {
    crs$wkt
  }

  x <- update_params(
    x,
    outFields = fields,
    where = where,
    crs = crs,
    ...
  )

  collect_layer(x)
}

