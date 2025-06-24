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
#' @param col_select Default `NULL`. A character vector of the field names to be
#'   returned. By default, all fields are returned.
#' @param n_max Defaults to `Inf` or an option set with
#'   `options("arcgislayers.n_max" = <max records>)`. Maximum number of records
#'   to return.
#' @param col_names Default `TRUE`. Column names or name handling rule.
#'   `col_names` can be `TRUE`, `FALSE`, `NULL`, or a character vector:
#'
#'  - If `TRUE`, use existing default column names for the layer or table.
#'  If `FALSE` or `NULL`, column names will be generated automatically: X1, X2,
#'  X3 etc.
#'  - If `col_names` is a character vector, values replace the existing column
#'  names. `col_names` can't be length 0 or longer than the number of fields in
#'  the returned layer.
#' @param alias Use of field alias values. Default `c("drop", "label",
#'   "replace"),`. There are three options:
#'
#'  - `"drop"`, field alias values are ignored.
#'  - `"label"`: field alias values are assigned as a label attribute for each field.
#'  - `"replace"`: field alias values replace existing column names. `col_names`
#' @inheritParams set_layer_aliases
#' @param fields Default `NULL`. a character vector of the field names to
#'   returned. By default all fields are returned. Ignored if `col_names` is
#'   supplied.
#' @inheritParams arc_select
#' @inheritParams arc_raster
#' @param ... Additional arguments passed to [arc_select()] if URL is a
#'   `FeatureLayer` or `Table` or [arc_raster()] if URL is an `ImageLayer`.
#' @returns An sf object, a `data.frame`, or an object of class `SpatRaster`.
#' @seealso [arc_select()]; [arc_raster()]
#' @examples
#' \dontrun{
#' furl <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3"
#'
#' # read entire service
#' arc_read(furl)
#'
#' # apply tolower() to column names
#' arc_read(url, name_repair = tolower)
#'
#' # use paste0 to prevent CRAN check NOTE
#' furl <- paste0(
#'   "https://sampleserver6.arcgisonline.com/arcgis/rest/services/",
#'   "EmergencyFacilities/FeatureServer/0"
#' )
#'
#' # use field aliases as column names
#' arc_read(furl, alias = "replace")
#'
#' # read an ImageServer directly
#' img_url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"
#'
#' arc_read(
#'   img_url,
#'   width = 100, height = 100,
#'   xmin = -71, ymin = 43,
#'   xmax = -67, ymax = 47.5,
#'   bbox_crs = 4326
#' )
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
  alias = "drop",
  token = arc_token()
) {
  # argument validation
  check_string(url, allow_empty = FALSE)
  check_character(fields, allow_null = TRUE)
  check_character(col_select, allow_null = TRUE)

  # be flexible with alias here
  alias <- alias %||% "drop"
  alias <- rlang::arg_match(alias, values = c("drop", "label", "replace"))

  is_valid_col_names_arg <- rlang::is_logical(col_names, 1) ||
    rlang::is_null(col_names) ||
    rlang::is_character(col_names)

  if (!is_valid_col_names_arg) {
    cli::cli_abort(
      "{.arg col_names} must be one of {.val TRUE}, {.val FALSE},\\
      {.val NULL}, or a character vector of the new column names"
    )
  }

  if (!rlang::is_integerish(n_max, 1)) {
    cli::cli_abort("{.arg n_max} must be a scalar integer.")
  }

  if (!is.null(token)) {
    obj_check_token(token)
  }

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

  if (identical(col_names, "alias")) {
    # Set alias to "replace" as name if col_names = "alias"
    alias <- "replace"
    col_names <- NULL

    lifecycle::deprecate_soft(
      "deprecated",
      what = "arc_read(col_names = \"can't be alias\")",
      with = "arc_read(alias = \"replace\")",
    )
  }

  if (
    identical(alias, "drop") || is.character(col_names) || isFALSE(col_names)
  ) {
    layer <- set_col_names(
      .data = layer,
      col_names = col_names,
      name_repair = name_repair
    )

    return(layer)
  }

  set_layer_aliases(
    .data = layer,
    .layer = x,
    name_repair = name_repair,
    alias = alias
  )
}

#' @noRd
check_col_names <- function(col_names, max_len, call = rlang::caller_env()) {
  if (rlang::is_logical(col_names) || is.null(col_names)) {
    return(invisible(NULL))
  }

  # check col_names input
  if (!is.character(col_names)) {
    cli::cli_abort(
      "{.arg col_names} must be `TRUE`, `FALSE`, `NULL`, or a character vector.",
      call = call
    )
  }

  col_names_len <- length(col_names)

  # Check col_names length
  if (col_names_len > 0 && col_names_len <= max_len) {
    return(invisible(NULL))
  }

  cli::cli_abort(
    "{.arg col_names} must be length {max_len}{? or shorter},
    not {col_names_len}.",
    call = call
  )
}

#' Handle col_names
#' @noRd
set_col_names <- function(
  .data,
  col_names = TRUE,
  name_repair = NULL,
  call = rlang::caller_env()
) {
  n_col <- ncol(.data)
  check_col_names(col_names, max_len = n_col, call = call)

  nm <- names(.data)
  sf_column <- attr(.data, "sf_column")
  field_nm <- setdiff(nm, sf_column)

  if (rlang::is_false(col_names)) {
    # Use X1, X2, etc. as names if col_names is FALSE
    col_names <- paste0("X", seq(n_col))
  }

  if (is.character(col_names)) {
    col_names_len <- length(col_names)

    if (col_names_len == n_col && !is.null(sf_column)) {
      # replace sf column name if lengths match
      .data <- sf::st_set_geometry(.data, col_names[[n_col]])
    } else {
      # if shorter fill missing field names using pattern, X1, X2, etc.
      if (col_names_len < length(field_nm)) {
        col_names <- c(
          col_names,
          paste0("X", seq(length(col_names) + 1, n_col))
        )
      }

      # but keep default sf column name
      col_names[[n_col]] <- sf_column %||% col_names[[n_col]]
    }

    nm <- col_names
  }

  repair_layer_names(.data, names = nm, name_repair = name_repair, call = call)
}

#' Set column labels or names based FeatureLayer or Table data frame field
#' aliases
#'
#' [set_layer_aliases()] can replace or label column names based on the the
#' field aliases from a corresponding `Table` or `FeatureLayer` object created
#' with `arc_open()`. Optionally repair names using [vctrs::vec_as_names()].
#'
#' @param .data A data frame returned by `arc_select()` or `arc_read()`.
#' @param .layer A Table or FeatureLayer object. Required.
#' @param alias Use of field alias values. Defaults to `"replace"`. There are two
#'   options:
#'
#'  - `"label"`: field alias values are assigned as a label attribute for each field.
#'  - `"replace"`: field alias values replace existing column names.
#' @param name_repair Default `"unique"`. See [vctrs::vec_as_names()] for
#'   details. If `name_repair = NULL` and `alias = "replace"` may include
#'   invalid names.
#' @inheritParams rlang::args_error_context
#' @export
#' @returns
#' A data.frame. When `alias = "replace"`, the column names are modified.
#' When `alias = "label"` each column has a new `label` attribute.
#'
#' @examples
#' furl <- paste0(
#'   "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/",
#'   "rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"
#' )
#'
#' # open the feature service
#' flayer <- arc_open(furl)
#'
#' # select first five rows
#' five_counties <- arc_select(flayer, n_max = 5)
#'
#' # add aliases
#' with_aliases <- set_layer_aliases(five_counties, flayer)
#'
#' # preview the new names
#' str(with_aliases, give.attr = FALSE)
set_layer_aliases <- function(
  .data,
  .layer,
  name_repair = "unique",
  alias = c("replace", "label"),
  call = rlang::caller_env()
) {
  check_data_frame(.data)
  check_inherits_any(.layer, c("FeatureLayer", "Table", "ImageServer"))
  alias <- rlang::arg_match(alias, error_call = call)
  nm <- names(.data)
  sf_column <- attr(.data, "sf_column")
  # get unnamed alias values
  alias_val <- unname(pull_field_aliases(.layer)[setdiff(nm, sf_column)])

  if (alias == "replace") {
    # Return if alias values are identical to the existing field names
    # NOTE: alias values may not be valid names
    nm <- alias_val
  }

  # geometry columns don't include an alias so keep any existing sf column
  nm[[ncol(.data)]] <- sf_column %||% nm[[ncol(.data)]]

  .data <- repair_layer_names(
    .data,
    names = nm,
    name_repair = name_repair,
    call = call
  )

  if (alias == "replace" && rlang::is_null(name_repair)) {
    return(.data)
  }

  alias_val <- rlang::set_names(
    alias_val,
    setdiff(names(.data), attr(.data, "sf_column"))
  )

  label_layer_fields(.data, values = alias_val)
}

#' Repair layer names using `vctrs::vec_as_names` and `rlang::set_names`
#' @noRd
repair_layer_names <- function(
  layer,
  names = NULL,
  name_repair = "unique",
  call = rlang::caller_env()
) {
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
  values
) {
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

#' Set coded values for FeatureLayer or Table data frame
#'
#' [encode_field_values()] can replace column values based on `codedValue`
#' type field domains from a corresponding `Table` or `FeatureLayer` object
#' created with `arc_open()`.
#'
#' @param .data A data frame returned by `arc_select()` or `arc_read()`.
#' @param .layer A Table or FeatureLayer object. Required.
#' @param field Optional character vector with names of fields to replace.
#'   Fields that do not have coded value domains are ignored. Defaults to `NULL`
#'   to replace or label all fields with coded value domains.
#' @param codes Use of field alias values. Defaults to `"replace"`.
#' There are two options:
#'
#'  - `"replace"`: coded values replace existing column values.
#'  - `"label"`: coded values are applied as value labels via a `"label"` attribute.
#' @inheritParams rlang::args_error_context
#' @export
#' @examples
#' \donttest{
#' layer <- arc_open(
#'   "https://geodata.baltimorecity.gov/egis/rest/services/Housing/dmxOwnership/MapServer/0"
#' )
#'
#' res <- arc_select(
#'   layer,
#'   n_max = 100,
#'   where = "RESPAGCY <> '  '",
#'   fields = "RESPAGCY"
#' )
#' encoded <- encode_field_values(res, layer)
#' table(encoded$RESPAGCY)
#' }
#' @returns
#' A data.frame with fields encoded with their respective domains.
encode_field_values <- function(
  .data,
  .layer,
  field = NULL,
  codes = c("replace", "label"),
  call = rlang::caller_env()
) {
  check_data_frame(.data)
  check_character(field, allow_null = TRUE)
  check_inherits_any(.layer, c("FeatureLayer", "Table", "ImageServer"))

  values <- pull_coded_values(.layer, field = field, call = call)

  codes <- rlang::arg_match(codes, error_call = call)

  # Check if coded values is an empty list
  if (rlang::is_empty(values)) {
    message <- "{.arg .layer} does not contain any coded values."

    if (!is.null(field)) {
      message <- "{.arg field} {.val {field}} does not contain any coded values."
    }

    cli::cli_warn(message)
    return(.data)
  }

  # Replace column values by default
  if (codes == "replace") {
    for (col in names(values)) {
      # Coerce numeric columns to character
      col_val <- as.character(.data[[col]])

      # Replace column values if not all missing or empty strings
      miss_val <- is.na(col_val) | col_val == ""
      if (any(!miss_val)) {
        replace_val <- values[[col]]
        col_val[!miss_val] <- replace_val[col_val[!miss_val]]
        .data[[col]] <- col_val
      }
    }

    return(.data)
  }

  # Label column values using new_labelled_col helper
  for (col in names(values)) {
    .data[[col]] <- new_labelled_col(
      .data[[col]],
      labels = rlang::set_names(
        names(values[[col]]),
        values[[col]]
      ),
      call = call
    )
  }

  .data
}

#' Set value labels compatible w/ `haven::labelled` package
#' @noRd
new_labelled_col <- function(
  x,
  labels = NULL,
  label = NULL,
  ...,
  class = character(),
  call = rlang::caller_env()
) {
  rlang::check_installed("vctrs", call = call)

  vctrs::new_vctr(
    x,
    labels = rlang::set_names(labels, names(labels)),
    label = label,
    ...,
    class = c(class, "haven_labelled"),
    inherit_base_type = TRUE
  )
}
