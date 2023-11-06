#' Extract a layer from a Feature or Map Server
#'
#' These helpers provide easy access to the layers contained in a
#' `FeatureServer` or `MapServer`.
#'
#' @param x an object of class `FeatureServer` or `MapServer`
#' @param id default `NULL`. A numeric vector of unique ID of the layer you want to retrieve. This is a scalar in `get_layer()`.
#' @param name default `NULL`. The name associated with the layer you want to retrieve. `name` is mutually exclusive with `id`. This is a scalar in `get_layer()`.
#' @inheritParams arc_open
#' @details
#'
#' The `id` and `name` arguments must match the field values of the respective names as seen in the output of `list_items()`
#'
#' @returns
#'
#' - `get_layer()` returns a single `FeatureLayer` or `Table` based on its ID
#' - `get_layers()` returns a list of the items specified by the `id` or `name` argument
#' - `get_all_layers()` returns a named `list` with an element `layers` and `tables`.
#'     Each a list containing `FeatureLayer` and `Table`s respectively.
#'
#' @export
get_layer <- function(x, id = NULL, name = NULL, token = Sys.getenv("ARCGIS_TOKEN")) {

  # check for mutual exclusivity between id and name
  if (is.null(id) && is.null(name)) {
    cli::cli_abort("{.arg id} or {.arg name} must be provided.")
  } else if (!is.null(id) && !is.null(name)) {
    cli::cli_abort(
      c(
        "{.arg id} and {.arg name} are mutually exclusive.",
        i = "Provide only {.arg id} or {.arg name}"
      )
    )
  } else if (!((length(id) == 1) || (length(name) == 1))) {
    cli::cli_abort("{.arg id} and {.arg name} must be of length 1.")
  }

  if (!is.null(name)) {

    # grab both table and layer names to check agains
    layer_names <- x[["layers"]][["name"]]
    table_names <- x[["tables"]][["name"]]

    # check if name is present as a table or layer
    is_layer_name <- name %in% layer_names
    is_table_name <- name %in% table_names

    # error if not found
    if (all(!is_layer, !is_table)) {
      cli::cli_abort("{.arg name} not available in {.code {c(layer_names, table_names)}}")
    }

    # grab layer ids
    layer_ids <- x[["layers"]][["id"]]

    # fetch the index
    item_url <- file.path(x[["url"]], layer_ids[which(layer_names == name)])

  } else if (!is.null(id)) {
    layer_ids <- x[["layers"]][["id"]]
    table_ids <- x[["tables"]][["id"]]

    all_ids <- list(layers = layer_ids, tables = table_ids)

    # find matching index
    is_layer <- id %in% layer_ids
    is_table <- id %in% table_ids

    if (all(!is_layer, !is_table)) {
      stop("id ", id, " not in available IDs (", toString(unlist(all_ids)), ")")
    }

    item_url <- file.path(x[["url"]], id)
  }

  arc_open(item_url, token = token)

}


#' @rdname get_layer
#' @export
get_all_layers <- function(x, token = Sys.getenv("ARCGIS_TOKEN")) {
  layer_ids <- x[["layers"]][["id"]]
  table_ids <- x[["tables"]][["id"]]
  layers <- lapply(file.path(x[["url"]], layer_ids), arc_open, token = token)
  tables <- lapply(file.path(x[["url"]], table_ids), arc_open, token = token)

  compact(
    list(
      layers = setNames(layers, layer_ids),
      tables = setNames(tables, table_ids)
    )
  )
}


#' @export
#' @rdname get_layer
get_layers <- function(x, id = NULL, name = NULL, token = Sys.getenv("ARCGIS_TOKEN")) {
  if (is.null(id) && is.null(name)) {
    cli::cli_abort("{.arg id} or {.arg name} must be provided.")
  } else if (!is.null(id) && !is.null(name)) {
    cli::cli_abort(
      c(
        "{.arg id} and {.arg name} are mutually exclusive.",
        i = "Provide only {.arg id} or {.arg name}"
      )
    )
  }

  if (!is.null(id)) {
    # cast as integer
    id <- as.integer(id)

    # ensure that all elements of `id` are in the layers
    in_ids <- id %in% x[["layers"]][["id"]]

    # if not report and remove
    baddies <- id[!in_ids]

    if (length(baddies > 1)) {
      cli::cli_warn("Invalid ID{?s}: {.val {as.character(baddies)}}")
    }

    id <- id[in_ids]
    item_urls <- file.path(x[["url"]], id)
  } else if (!is.null(name)) {
    valid_names <- x[["layers"]][["name"]]

    # validate names
    in_names <- name %in% valid_names
    baddies <- name[!in_names]

    if (length(baddies > 1)) {
      cli::cli_warn("Invalid item names{?s}: {.val {baddies}}")
    }

    # create lookup table for fetching ids
    lu <- setNames(x[["layers"]][["id"]], valid_names)

    item_urls <- file.path(
      x[["url"]],
      unname(lu[name[in_names]])
    )
  }

  if (length(item_urls) < 1) {
    cli::cli_abort(
      c(
        "No valid items to return.",
        i = "Ensure 1 or more valid {.arg id} or {.arg name} value is provided."
      )
    )
  }

  lapply(item_urls, arc_open)
}
