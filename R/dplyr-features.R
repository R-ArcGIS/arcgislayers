
# Select ------------------------------------------------------------------


#' @keywords internal
#' @rdname dplyr
select.FeatureLayer <- function(x, ...) {

  # capture valeus passed to dots
  select_quos <- rlang::quos(...)

  col_names <- tolower(x[[c("fields", "name")]])

  select_names <- vapply(select_quos, rlang::as_name, character(1))

  in_index <- tolower(select_names) %in% col_names

  if (any(!in_index)) {
    cli::cli_abort("Variable{?s} {.var {select_names[!in_index]}} not found in {.arg x}")
  }

  out_fields <- paste(select_names, collapse = ",")

  attr(x, "query")[["outFields"]] <- out_fields
  x
}


#' @rdname dplyr
select.Table <- select.FeatureLayer



# Filter ------------------------------------------------------------------

#' @keywords internal
#' @rdname dplyr
filter.FeatureLayer <- function(x, ...) {

  where_clause <- attr(x, "query")[["where"]]

  filt_quos <- rlang::quos(...)

  ptype_df <- arcgisutils::remote_ptype_tbl(x[["fields"]])

  lapply(filt_quos, dbplyr::partial_eval, ptype_df)

  where_clause <- c(where_clause, gsub("`", "", dbplyr::translate_sql(!!!filt_quos)))

  if (length(where_clause) > 1) where_clause <- paste(where_clause, collapse = " AND ")

  attr(x, "query")[["where"]] <- as.character(where_clause)
  x
}


# Table will use same filtering as feature layer. nothing special
#' @keywords internal
#' @rdname dplyr
filter.Table <- filter.FeatureLayer



# Collect -----------------------------------------------------------------


#> we want to return all fields if nothing specified
#> so we need to check if null
#> minimum params we need:
#>  - where 1=1
#>  - output = fgeojson
#>  - token

#' dplyr methods
#'
#' @details
#'
#' The Feature Layer method of `collect()` will overwrite the `returnGeometry` parameter if set with `update_params()`. Use the `geometry` argument in `collect()` to set the parameter.
#'
#' @aliases dplyr
#' @rdname dplyr
#' @keywords internal
collect.FeatureLayer <- collect_layer
#' @keywords internal
#' @rdname dplyr
collect.Table <- collect_layer

