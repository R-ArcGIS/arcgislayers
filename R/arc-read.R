#' Read an ArcGIS FeatureLayer, Table, or ImageServer
#'
#' [arc_read()] combines the functionality of [arc_open()] with [arc_select()]
#' or [arc_raster()] to read an ArcGIS `FeatureLayer`, `Table`, or `ImageServer` to an
#' `sf` or `SpatRaster` object. Optionally, set, check, or modify
#' names for the returned data frame or sf object using the `col_names` and
#' `name_repair` parameters.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams arc_open
#' @param col_names Default `TRUE`. If `TRUE`, use the default column names for
#'   the feature. If `col_names` is a character vector with the same length as
#'   the number of columns in the layer, the default names are replaced with the
#'   new names. If `col_names` has one fewer name than the default column names,
#'   the existing sf column name is retained.
#' @param col_select Default `NULL`. A character vector of the field names to be
#'   returned. By default, all fields are returned.
#' @param alias Default "drop". Supported options: `c("drop", "label",
#'   "replace")`. If "drop", field alias values are ignored. If "label", field
#'   alias values are assigned as a label attribute for each field. If replace,
#'   field alias values are used as the column names.
#' @param n_max Defaults to 10000 or an option set with
#'   `options("arcgislayers.n_max" = <max records>)`. Maximum number of records
#'   to return.
#' @inheritParams arc_select
#' @inheritParams arc_raster
#' @param name_repair Default `"unique"`. See [vctrs::vec_as_names()] for
#'   details. If `name_repair = NULL`, names are set directly.
#' @param ... Additional arguments passed to [arc_select()] if URL is a
#'   `FeatureLayer` or `Table` or [arc_raster()] if URL is an `ImageLayer`.
#' @returns An sf object, a `data.frame`, or an object of class `SpatRaster`.
#' @examples
#' \dontrun{
#'   furl <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census#' /MapServer/3"
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
#'  arc_read(furl, alias = "replace")
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
    n_max = getOption("arcgislayers.n_max", default = 10000),
    name_repair = "unique",
    crs = NULL,
    ...,
    fields = NULL,
    alias = c("drop", "label", "replace"),
    token = arc_token()
) {
  service <- arc_open(url = url, token = token)

  crs <- crs %||% sf::st_crs(service)

  # if the server is an ImageServer we use arc_raster
  if (inherits(service, "ImageServer")) {
    layer <- arc_raster(
      x = service,
      ...,
      crs = crs,
      token = token
    )

    return(layer)

  } else if (!obj_is_layer(service)) {
    # if it is not a layer we abort
    # implicitly checks for Layer type and permits continuing
    cli::cli_abort(
      c(
        "{.arg url} is not a supported type:
      {.val FeatureLayer}, {.val Table}, or {.val ImageServer}",
        "i" = "found {.val {class(service)}}"
      )
    )
  }

  layer <- arc_select(
    x = service,
    fields = col_select,
    crs = crs,
    n_max = n_max,
    token = token,
    ...
  )

  set_layer_col_names(
    layer,
    service = service,
    col_names = col_names,
    name_repair = name_repair,
    alias = alias
  )
}

#' Set names for layer or table
#'
#' @noRd
set_layer_col_names <- function(
    x,
    service = NULL,
    col_names = NULL,
    name_repair = NULL,
    alias = c("drop", "label", "replace"),
    call = rlang::caller_env()
) {

  # Use existing names by default
  field_nm <- names(x)
  replace_nm <- field_nm
  sf_column_nm <- attr(x, "sf_column")

  alias <- rlang::arg_match(alias, error_call = call)

  if (alias == "label") {
    # get fields from service object
    fields <- service[["fields"]]

    # filter alias values to selected fields
    fields <- fields[fields[["name"]] %in% field_nm, ]

    # get alias values
    alias_val <- fields[["alias"]]
  }

  if (is.character(col_names)) {
    if (identical(col_names, "alias")) {
      # Assign alias values as name if col_names = "alias"
      col_names <- alias_val
      lifecycle::signal_stage(
        "superseded",
        what = "arc_read(..., field = 'alias')",
        with = "arc_read(..., alias = 'replace')",
      )
    }

    if (alias == "replace") {
      # NOTE: alias values may not be valid names
      col_names <- alias_val
    }

    replace_nm <- col_names
  }

  replace_nm_len <- length(replace_nm)

  if (rlang::is_false(col_names)) {
    # Use X1, X2, etc. as names if col_names is FALSE
    replace_nm <- paste0("X", seq(to = replace_nm_len))
  }

  # If x is a sf object and sf column is not in names, check to ensure names
  # work with geometry column
  if (inherits(x, "sf") && sf_column_nm != replace_nm[[replace_nm_len]]) {
    field_nm_len <- length(field_nm)
    if (length(replace_nm) == field_nm_len) {
      # If same number of names as layer columns, use last name for geometry
      x <- sf::st_set_geometry(x, replace_nm[[length(field_nm)]])
    } else if (replace_nm_len == (field_nm_len - 1)) {
      # If same number of names as layer columns, use existing geometry name
      replace_nm <- c(replace_nm, sf_column_nm)
    }
  }

  if (!is.null(name_repair)) {
    rlang::check_installed("vctrs", call = call)
    nm <- vctrs::vec_as_names(
      names = replace_nm,
      repair = name_repair,
      repair_arg = "name_repair",
      call = call
    )
  }

  layer <- rlang::set_names(
    x,
    nm = replace_nm
  )

  if (alias != "label") {
    return(layer)
  }

  # Name alias values with layer names
  alias_val <- rlang::set_names(
    alias_val,
    nm = setdiff(replace_nm, attr(layer, "sf_column"))
  )

  set_layer_col_labels(
    layer,
    values = alias_val
  )
}

#' @noRd
set_layer_col_labels <- function(
    layer,
    values,
    safe = TRUE) {
  if (safe) {
    nm <- intersect(names(values), names(layer))
  }

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
