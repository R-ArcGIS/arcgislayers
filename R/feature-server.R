# TODO implement head method
# url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/hexagons_state/FeatureServer"
# fl <- feature_layer(file.path(url, 0))
# fl


#' Create a feature server
#'
#' @param url the url of a feature server
#' @examples
#' url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/hexagons_state/FeatureServer"
#' feature_server(url)
#' @export
feature_server <- function(url, token = Sys.getenv("ARCGIS_TOKEN")) {
  req <- httr2::request(url)
  meta <- fetch_layer_metadata(req, token = token)
  meta <- compact(meta)
  meta[["url"]] <- url
  structure(
    meta,
    class = "FeatureServer"
  )
}

#' @export
print.FeatureServer <- function(x, n, ...) {

  n_fts <- length(x[["layers"]][["id"]])
  n_tbls <- length(x[["tables"]][["id"]])
  cli::cli_text(
    cli::style_bold("<", paste(class(x), collapse = "/"), " <"),

    cli::style_bold(
      cli::style_italic(
        "{n_fts + n_tbls} Features"
      )),
    cli::style_bold(">>")
  )

  # identify CRS
  crs <- x[["spatialReference"]][["latestWkid"]]

  # if this doesn't catch crs, then it will be NULL and omitted
  if (is.null(crs)) crs <- x[["fullExtent"]][["spatialReference"]][["latestWkid"]]

  # crs and capabilities will always be printed
  to_print <- compact(
    list(
      "CRS" = crs,
      "Capabilities" = x[["capabilities"]]
    )
  )

  cli::cli_dl(to_print)

  # create layers
  lyr <- x[["layers"]]

  # LAYERS create and format vector to populate box
  box_ln <- character(nrow(lyr))

  # format
  for (i in 1:nrow(lyr)) {
    lx <- lyr[i, ]
    box_ln[i] <- cli::cli_fmt(cli::cli_text(("{.val {lx$id}}: {lx$name} {.field ({lx$geometryType})}")))
  }


  # Tables
  # if tables aren't missing populate
  if (!is.null(x[["tables"]])) {

    box_tbl <- character(nrow(x[["tables"]]))
    for (i in seq_along(box_tbl)) {
      lx <- x[["tables"]][i, ]
      box_tbl[i] <- cli::cli_fmt(
        cli::cli_text(("{.val {lx$id}}: {lx$name} {.field (Table)}"))
        )
    }
  } else {
    # if missing assign to null
    box_tbl <- NULL
  }


  # print boxx
  cat(
    cli::boxx(
      c(box_ln, box_tbl),
      #padding = 0,
      header = cli::style_italic("Features"),
      footer = cli::cli_fmt(cli::cli_text(cli::style_italic("{x$serviceDescription}"))),
      border_style = "single",
      padding = c(0, 1, 0, 2)
    )
  )

  invisible(x)
}


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
#' @export
get_layer <- function(x, id = 0, token = "") {
  layer_ids <- x[["layers"]][["id"]]
  table_ids <- x[["tables"]][["id"]]

  all_ids <- list(layers = layer_ids, tables = table_ids)

  # find matching index
  is_layer <- id %in% layer_ids
  is_table <- id %in% table_ids

  if (all(!is_layer, !is_table)) {
    cli::cli_abort("{.var id ({id})} not in available IDs ({all_ids})")
  }

  # using values from is_layer to determine which function is used
  switch(
    as.character(is_layer),
    "TRUE" = feature_layer(file.path(x[["url"]], id), token = token),
    "FALSE" = feature_table(file.path(x[["url"]], id), token = token)
  )
}


#' @rdname get_layer
#' @export
get_all_layers <- function(x, token = "") {
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


