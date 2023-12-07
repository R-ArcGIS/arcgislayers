
# Select ------------------------------------------------------------------

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

select.Table <- select.FeatureLayer

# Filter ------------------------------------------------------------------

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


filter.Table <- filter.FeatureLayer

# Collect -----------------------------------------------------------------

collect.FeatureLayer <- collect_layer
collect.Table <- collect_layer
