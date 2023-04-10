# Feature Server Helpers --------------------------------------------------


#' Extract a feature layer
#'
#' From a Feature Server, extract a feature layer.
#'
#' @returns
#'
#' `get_layer()` returns a single `FeatureLayer` or `Table` based on its ID
#' `get_all_layers()` returns a named `list` with an element `layers` and `tables`. Each a list containing `FeatureLayer` and `Table`s respectively.
#'
#'
get_layer <- function(x, id = 0, token = Sys.getenv("ARCGIS_TOKEN")) {
  layer_ids <- x[["layers"]][["id"]]
  table_ids <- x[["tables"]][["id"]]

  all_ids <- list(layers = layer_ids, tables = table_ids)

  # find matching index
  is_layer <- id %in% layer_ids
  is_table <- id %in% table_ids

  if (all(!is_layer, !is_table)) {
    stop("id ", id, " not in available IDs (", toString(unlist(all_ids)), ")")
  }

  # using values from is_layer to determine which function is used
  switch(
    as.character(is_layer),
    "TRUE" = feature_layer(file.path(x[["url"]], id), token = token),
    "FALSE" = feature_table(file.path(x[["url"]], id), token = token)
  )
}


#' @rdname get_layer
#'
get_all_layers <- function(x, token = Sys.getenv("ARCGIS_TOKEN")) {
  layer_ids <- x[["layers"]][["id"]]
  table_ids <- x[["tables"]][["id"]]
  layers <- lapply(file.path(x[["url"]], layer_ids), feature_layer, token = token)
  tables <- lapply(file.path(x[["url"]], table_ids), feature_table, token = token)

  compact(
    list(
      layers = setNames(layers, layer_ids),
      tables = setNames(tables, table_ids)
    )
  )
}


