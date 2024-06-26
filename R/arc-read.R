#' Read an ArcGIS FeatureLayer, Table, or ImageServer
#'
#' [arc_read()] combines the functionality of [arc_open()] with [arc_select()]
#' or [arc_raster()] to read an ArcGIS `FeatureLayer`, `Table`, or `ImageServer`
#' to an `sf` or `SpatRaster` object. Optionally, set, check, or modify names
#' for the returned data frame or sf object using the `col_names` and
#' `name_repair` parameters. For ease of use and convenience, [arc_read()]
#' allows users to access and query a FeatureLayer, Table, or ImageServer with a
#' single function call instead of combining [arc_open()] and [arc_select()].
#' The conventions of `col_select` are based on functions for reading tabular
#' data in the `{readr}` package.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams arc_open
#' @param col_names Default `TRUE`. Column names or name handling rule.
#'   `col_names` can be `TRUE`, `FALSE`, `NULL`, or a character vector:
#'
#'  - If `TRUE`, use existing default column names for the layer or table.
#'  If `FALSE` or `NULL`, column names will be generated automatically: X1, X2,
#'  X3 etc.
#'  - If `col_names` is a character vector, values replace the existing column
#'  names. `col_names` can't be length 0 or longer than the number of fields in
#'  the returned layer.
#' @param col_select Default `NULL`. A character vector of the field names to be
#'   returned. By default, all fields are returned.
#' @param n_max Defaults to `Inf` or an option set with
#'   `options("arcgislayers.n_max" = <max records>)`. Maximum number of records
#'   to return.
#' @param alias Use of field alias values. Default `c("drop", "label",
#'   "replace"),`. There are three options:
#'
#'  - `"drop"`, field alias values are ignored.
#'  - `"label"`: field alias values are assigned as a label attribute for each field.
#'  - `"replace"`: field alias values replace existing column names. `col_names`
#'  must `TRUE` for this option to be applied.
#' @param fields Default `NULL`. a character vector of the field names to
#'   returned. By default all fields are returned. Ignored if `col_names` is
#'   supplied.
#' @inheritParams arc_select
#' @inheritParams arc_raster
#' @param name_repair Default `"unique"`. See [vctrs::vec_as_names()] for
#'   details. If `name_repair = NULL`, names are set directly.
#' @param ... Additional arguments passed to [arc_select()] if URL is a
#'   `FeatureLayer` or `Table` or [arc_raster()] if URL is an `ImageLayer`.
#' @returns An sf object, a `data.frame`, or an object of class `SpatRaster`.
#' @seealso [arc_select()]; [arc_raster()]
#' @examples
#' \dontrun{
#'   furl <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3"
#'
#'   # read entire service
#'   arc_read(furl)
#'
#'   # apply tolower() to column names
#'   arc_read(url, name_repair = tolower)
#'
#'   # use paste0 to prevent CRAN check NOTE
#'   furl <- paste0(
#'     "https://sampleserver6.arcgisonline.com/arcgis/rest/services/",
#'     "EmergencyFacilities/FeatureServer/0"
#'   )
#'
#'  # use field aliases as column names
#'  arc_read(furl, col_names = "alias")
#'
#'  # read an ImageServer directly
#'  img_url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"
#'
#'  arc_read(
#'    img_url,
#'    width = 100, height = 100,
#'    xmin = -71, ymin = 43,
#'    xmax = -67, ymax = 47.5,
#'    bbox_crs = 4326
#'  )
#'
#' }
#' @export
arc_read <- function(
    url,
    col_names = TRUE,
    col_select = NULL,
    n_max = Inf,
    name_repair = "unique",
    crs = NULL,
    ...,
    fields = NULL,
    alias = c("drop", "label", "replace"),
    token = arc_token()) {
  x <- arc_open(url = url, token = token)

  # Default crs must be NULL since crs can't be taken from x at execution
  crs <- crs %||% sf::st_crs(x)

  # if the server is an ImageServer use arc_raster
  if (inherits(x, "ImageServer")) {
    layer <- arc_raster(
      x = x,
      ...,
      crs = crs,
      token = token
    )

    return(layer)
  } else if (!obj_is_layer(x)) {
    # if it is not a layer we abort
    # implicitly checks for Layer type and permits continuing
    cli::cli_abort(
      c(
        "{.arg url} is not a supported type:
      {.val FeatureLayer}, {.val Table}, or {.val ImageServer}",
        "i" = "found {.val {class(x)}}"
      )
    )
  }

  # TODO: Should this pattern be implemented for arc_select?
  if (is.infinite(n_max) && is.numeric(getOption("arcgislayers.n_max"))) {
    n_max <- getOption("arcgislayers.n_max")
  }

  layer <- arc_select(
    x = x,
    fields = col_select %||% fields,
    crs = crs,
    n_max = n_max,
    token = token,
    ...
  )

  set_layer_col_names(
    layer,
    col_names = col_names,
    name_repair = name_repair,
    alias = alias,
    x = x
  )
}

#' Set names for layer or table
#' @noRd
set_layer_col_names <- function(
    layer,
    col_names = TRUE,
    name_repair = NULL,
    alias = c("drop", "label", "replace"),
    x = NULL,
    call = rlang::caller_env()) {
  # check col_names input
  if (!is.null(col_names) && !rlang::is_logical(col_names) && !is.character(col_names)) {
    cli::cli_abort(
      "{.arg col_names} must be `TRUE`, `FALSE`, `NULL`, or a character vector.",
      call = call
    )
  }

  alias <- rlang::arg_match(alias, error_call = call)

  # skip col_names and alias handling if possible
  if (rlang::is_true(col_names) && alias == "drop") {
    return(repair_layer_names(layer, name_repair = name_repair, call = call))
  }

  existing_nm <- names(layer)
  n_col <- ncol(layer)
  sf_column <- attr(layer, "sf_column")

  # Use existing names by default
  replace_nm <- existing_nm

  if (alias != "drop" || identical(col_names, "alias")) {
    # get alias values and drop names
    alias_val <- pull_field_aliases(x)[setdiff(existing_nm, sf_column)]
    alias_val <- as.character(alias_val)

    if (alias == "replace") {
      # NOTE: alias values may not be valid names
      replace_nm <- alias_val
    }
  }

  if (is.character(col_names)) {
    col_names_len <- length(col_names)

    # Check col_names length
    if ((col_names_len > n_col) || col_names_len == 0) {
      cli::cli_abort(
        "{.arg col_names} must be length {n_col}{? or shorter}, not {col_names_len}.",
        call = call
      )
    }

    if (identical(col_names, "alias")) {
      # Assign alias values as name if col_names = "alias"
      col_names <- alias_val
      lifecycle::signal_stage(
        "superseded",
        what = "arc_read(col_names = \"can't be alias\")",
        with = "arc_read(alias = \"replace\")",
      )
    }

    replace_nm <- col_names
  }

  if (rlang::is_false(col_names) || is.null(col_names)) {
    # Use X1, X2, etc. as names if col_names is FALSE
    replace_nm <- paste0("X", seq_along(existing_nm))
  }

  replace_nm_len <- length(replace_nm)

  if (replace_nm_len < n_col) {
    # fill missing field names using pattern, X1, X2, etc.
    replace_nm <- c(replace_nm, paste0("X", seq(replace_nm_len + 1, n_col)))

    # But keep the default sf column name
    if (inherits(layer, "sf")) {
      replace_nm[[n_col]] <- sf_column
    }
  }

  layer <- repair_layer_names(
    layer,
    names = replace_nm,
    name_repair = name_repair,
    call = call
  )

  if (alias != "label") {
    return(layer)
  }

  # Name alias values with layer names
  alias_val <- rlang::set_names(
    alias_val,
    nm = setdiff(replace_nm, sf_column)
  )

  label_layer_fields(layer, values = alias_val)
}

#' Repair layer names using `vctrs::vec_as_names` and `rlang::set_names`
#' @noRd
repair_layer_names <- function(
    layer,
    names = NULL,
    name_repair = "unique",
    call = rlang::caller_env()) {
  names <- names %||% colnames(layer)

  if (!is.null(name_repair)) {
    rlang::check_installed("vctrs", call = call)

    names <- vctrs::vec_as_names(
      names = names,
      repair = name_repair,
      repair_arg = "name_repair",
      call = call
    )
  }

  rlang::set_names(layer, nm = names)
}

#' Apply a label attribute value to each column of layer
#' @noRd
label_layer_fields <- function(
    layer,
    values) {
  nm <- intersect(names(values), colnames(layer))

  for (v in nm) {
    label_attr(layer[[v]]) <- values[[v]]
  }

  layer
}

#' Set label attribute
#' @seealso [labelled::set_label_attribute()]
#' @source <https://github.com/cran/labelled/blob/master/R/var_label.R>
#' @noRd
`label_attr<-` <- function(x, value) {
  attr(x, "label") <- value
  x
}
