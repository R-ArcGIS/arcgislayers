#' Read a ArcGIS FeatureLayer, Table, or ImageServer
#'
#' [arc_read()] combines the functionality of [arc_open()] with [arc_select()]
#' or [arc_raster()] to read an ArcGIS FeatureLayer, Table, or ImageServer to a
#' `sf` object or object of class `SpatRaster`. `n_max` defaults to 10000 to
#' avoid unintentionally reading an entire layer with a very large number of
#' features.
#'
#' @inheritParams arc_open
#' @param col_select Columns to select. Alternate argument for specifying fields
#'   if fields is `NULL`.
#' @param col_names If `TRUE`, use the default column names for the feature. If
#'   `col_names` is a character function with the same length as the number of
#'   columns in the layer, the default names are replaced with the character
#'   vector. If `col_names` is one less than the length of the default or if
#'   `col_names` if `FALSE`, the existing sf column name is retained. If
#'   `col_names` is the string "alias", names are set to match the available
#'   alias names for the layer.
#' @inheritParams arc_select
#' @inheritParams arc_raster
#' @param fields Fields to return. Ignored if `col_select` is supplied.
#' @param name_repair See [vctrs::vec_as_names()] for details.
#' @param ... Additional arguments passed to [arc_select()] or [arc_raster()]
#' @examples
#' if (interactive()) {
#'   url <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3"
#'
#'   arc_read(url)
#'
#'   img_url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"
#'
#'   arc_read(
#'     img_url,
#'     width = 1000, height = 1000,
#'     xmin = -71, ymin = 43,
#'     xmax = -67, ymax = 47.5,
#'     bbox_crs = 4326
#'   )
#' }
#' @export
arc_read <- function(url,
                     col_names = TRUE,
                     col_select = NULL,
                     n_max = getOption("arcgislayers.n_max", default = 10000),
                     name_repair = "unique",
                     crs = NULL,
                     ...,
                     fields = NULL,
                     token = Sys.getenv("ARCGIS_TOKEN")) {
  service <- arc_open(url = url, token = token)

  crs <- crs %||% sf::st_crs(service)

  if (!obj_is_layer(service)) {
    layer <- arc_raster(
      x = service,
      ...,
      crs = crs,
      token = token
    )

    return(layer)
  }

  if (attr(service, "n") > n_max) {
    cli::cli_alert_warning(
      "{.arg n_max} is set to {n_max}, less than the {attr(service, 'n')}
      features available from this service."
    )
  }

  layer <- arc_select(
    x = service,
    fields = col_select %||% fields,
    crs = crs,
    n_max = n_max,
    token = token,
    ...
  )

  set_layer_names(
    layer,
    col_names = col_names,
    name_repair = name_repair,
    alias = service[["fields"]][["alias"]]
  )

}

#' @noRd
#' @importFrom vctrs vec_as_names
set_layer_names <- function(x,
                            col_names = NULL,
                            name_repair = NULL,
                            alias = NULL,
                            ...) {
  layer_nm <- names(x)
  nm <- layer_nm
  sf_column_nm <- attributes(x)[["sf_column"]]

  if (is.character(col_names)) {
    if (identical(col_names, "alias") && is.character(alias)) {
      col_names <- alias
    }

    nm <- col_names
  }

  nm_len <- length(nm)

  if (rlang::is_false(col_names)) {
    nm <- paste0("X", seq(to = nm_len))
  }

  if (inherits(x, "sf") && sf_column_nm != nm[[nm_len]]) {
    layer_nm_len <- length(layer_nm)
    if (length(nm) == layer_nm_len) {
      x <- sf::st_set_geometry(x, nm[[length(layer_nm)]])
    } else if (length(nm) == (layer_nm_len - 1)) {
      nm <- c(nm, sf_column_nm)
    }
  }

  if (!is.null(name_repair)) {
    nm <- vctrs::vec_as_names(
      names = nm,
      repair = name_repair,
      repair_arg = "name_repair"
    )
  }

  rlang::set_names(
    x,
    nm = nm
  )
}
