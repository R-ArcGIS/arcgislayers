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
#' `r lifecycle::badge("experimental")`
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
#' @examples
#' if (interactive()) {
#'   # FeatureServer
#'   furl <- paste0(
#'     "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/",
#'     "PLACES_LocalData_for_BetterHealth/FeatureServer"
#'   )
#'
#'   fserv <- arc_open(furl)
#'
#'   fserv
#'   get_layer(fserv, 0)
#'   get_layers(fserv, name = c("Tracts", "ZCTAs"))
#'   get_all_layers(fserv)
#' }
get_layer <- function(x, id = NULL, name = NULL, token = arc_token()) {
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

  UseMethod("get_layer")
}

#' @export
get_layer.default <- function(x, id = NULL, name = NULL, token = arc_token()) {

  if (!is.null(name)) {

    # grab both table and layer names to check agains
    layer_names <- x[["layers"]][["name"]]
    table_names <- x[["tables"]][["name"]]

    # check if name is present as a table or layer
    is_layer_name <- name %in% layer_names
    is_table_name <- name %in% table_names

    # error if not found
    if (all(!is_layer_name, !is_table_name)) {
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

#' @export
get_layer.GroupLayer <- function(
    x,
    id = NULL,
    name = NULL,
    token = arc_token()
) {
  if (!is.null(name)) {

    layer_names <- x[["subLayers"]][["name"]]

    # check if name is present as a table or layer
    is_layer_name <- name %in% layer_names

    # error if not found
    if (!is_layer_name) {
      cli::cli_abort("{.arg name} not available in {.code {layer_names}}")
    }

    # grab layer ids
    layer_ids <- x[["subLayers"]][["id"]]

    # match item id
    item_id <- layer_ids[which(layer_names == name)]

    # the new item_url
    item_url <- sub("\\d+$", item_id, x[["url"]])

  } else if (!is.null(id)) {
    layer_ids <- x[["subLayers"]][["id"]]

    # find matching index
    is_layer <- id %in% layer_ids

    if (!is_layer) {
      cli::cli_abort(
        paste0("{.arg id} ", id, " not in available IDs (", toString(unlist(layer_ids)), ")")
      )
    }

    item_url <- sub("\\d+$", id, x[["url"]])
  }

  arc_open(item_url, token = token)
}


#' @rdname get_layer
#' @export
get_all_layers <- function(x, token = arc_token()) {
  UseMethod("get_all_layers")
}

#' @export
get_all_layers.default <- function(x, token = arc_token()) {
  layer_ids <- x[["layers"]][["id"]]
  table_ids <- x[["tables"]][["id"]]
  layers <- lapply(file.path(x[["url"]], layer_ids), arc_open, token = token)
  tables <- lapply(file.path(x[["url"]], table_ids), arc_open, token = token)

  compact(
    list(
      layers = stats::setNames(layers, layer_ids),
      tables = stats::setNames(tables, table_ids)
    )
  )
}

#' @export
get_all_layers.GroupLayer <- function(x, token = arc_token()) {
  all_layer_ids <- x[["subLayers"]][["id"]]

  all_layer_paths <- vapply(
    all_layer_ids,
    function(.x) sub("\\d+$", .x, x[["url"]]),
    character(1)
  )

  lapply(all_layer_paths, arc_open)
}


#' @export
#' @rdname get_layer
get_layers <- function(
    x,
    id = NULL,
    name = NULL,
    token = arc_token()
) {
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

  UseMethod("get_layers")
}

#' @export
get_layers.default <- function(x, id = NULL, name = NULL, token = arc_token()) {

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

    if (length(baddies) > 1) {
      cli::cli_warn("Invalid item names{?s}: {.val {baddies}}")
    }

    # create lookup table for fetching ids
    lu <- stats::setNames(x[["layers"]][["id"]], valid_names)

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


#' @export
get_layers.GroupLayer <- function(
    x,
    id = NULL,
    name = NULL,
    token = arc_token()
) {
  if (!is.null(id)) {
    # cast as integer
    id <- as.integer(id)

    # ensure that all elements of `id` are in the layers
    in_ids <- id %in% x[["subLayers"]][["id"]]

    # if not report and remove
    baddies <- id[!in_ids]

    if (length(baddies) > 1) {
      cli::cli_warn("Invalid ID{?s}: {.val {as.character(baddies)}}")
    }

    all_layer_ids <- id[in_ids]

    item_urls <- vapply(
      all_layer_ids,
      function(.x) sub("\\d+$", .x, x[["url"]]),
      character(1)
    )

  } else if (!is.null(name)) {
    valid_names <- x[["subLayers"]][["name"]]

    # validate names
    in_names <- name %in% valid_names
    baddies <- name[!in_names]

    if (length(baddies) > 1) {
      cli::cli_warn("Invalid item names{?s}: {.val {baddies}}")
    }

    # create lookup table for fetching ids
    lu <- stats::setNames(x[["subLayers"]][["id"]], valid_names)

    all_layer_ids <- unname(lu[name[in_names]])

    item_urls <- vapply(
      all_layer_ids,
      function(.x) sub("\\d+$", .x, x[["url"]]),
      character(1)
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
