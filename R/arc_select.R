# https://developers.arcgis.com/rest/services-reference/enterprise/query-feature-service-.htm

arc_select <- function(x, fields, where_clause, ...) {
  UseMethod("arc_select")
}

arc_select.FeatureLayer <- function(
    x,
    fields,
    where_clause,
    sr = st_crs(x),
    token = Sys.getenv("ARCGIS_TOKEN"),
    ...
    ) {
  #outSR is the query parameter for sr arg
  obj_class <- class(x)

  stopifnot(
    "`x` must be a `FeatureLayer` or a `Table`" = inherits(x, c("FeatureLayer"))
    )

  # validate fetch SRID
  sr <- validate_crs(sr)[["spatialReference"]][["wkid"]]

  query <- attr(x, "query")

  # if missing fields arg: handle
  if (missing(fields) && is.null(query$outFields)) {
    fields <- "*"
  } else if (missing(fields) && !is.null(query$outFields)) {
    fields <- query$outFields
  }

  # if missing where_clause arg: handle
  if (missing(where_clause) && is.null(query$where)) {
    where_clause <- "1=1"
  } else if (missing(where_clause) && !is.null(query$where)) {
    where_clause <- query$where
  }

  x <- update_params(
    x,
    outFields = paste0(fields, collapse = ","),
    where = where_clause,
    outSR = sr,
    ...
      )

  collect.FeatureLayer(x, token = token)

}


arc_select.Table <- function(x, fields, where_clause, token = Sys.getenv("ARCGIS_TOKEN"), ...) {

  obj_class <- class(x)

  stopifnot(
    "`x` must be a `FeatureLayer` or a `Table`" = inherits(x, c("Table"))
  )

  query <- attr(x, "query")

  # if missing fields arg: handle
  if (missing(fields) && is.null(query$outFields)) {
    fields <- "*"
  } else if (missing(fields) && !is.null(query$outFields)) {
    fields <- query$outFields
  }

  # if missing where_clause arg: handle
  if (missing(where_clause) && is.null(query$where)) {
    where_clause <- "1=1"
  } else if (missing(where_clause) && !is.null(query$where)) {
    where_clause <- query$where
  }

  x <- update_params(
    x,
    outFields = paste0(fields, collapse = ","),
    where = where_clause,
    ...
  )

  collect.Table(x, token = token)
}
