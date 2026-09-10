# CLAUDE.md

## About

`arcgislayers` reads, writes, and publishes ArcGIS feature services, image services, and tables. It sits on `arcgisutils` (auth, EsriJSON, portal API) and `arcpbf` (protocol buffer parsing).

## Running tests

Tests hit live services. `skip_on_cran()` guards them, so run with `NOT_CRAN=true`:

```sh
NOT_CRAN=true R -q -e 'devtools::test()'
```

These files need an `ARCGIS_TOKEN` and fail without one: `test-attachments.R`, `test-adding-features.R`, `test-write-feature-layer.R`, `test-update-fields.R`, `test-delete-feats.R`, `test-create-feature-service.R`, `test-definition.R`. `test-empty-services.R` and `test-raster-fns.R` reference services that have since gone away. Get a baseline before assuming a failure is yours.

To test against unreleased `arcgisutils` or `arcpbf`, load them first:

```r
devtools::load_all("../arcgisutils")
devtools::load_all(".")
```

## Gotchas

### rlang standalone checks take `...` before their options

`check_character(x, ..., allow_null, arg, call)` and `check_number_whole(x, ..., min, max, ...)` put everything positional into `...` and drop it silently. `check_number_whole(n, 1, 100)` enforces nothing, and `check_character(x, allow_empty = FALSE)` is a no-op because `check_character()` has no such argument. Always name the option and confirm it exists in `R/import-standalone-types-check.R`.

### `rlang::is_empty()` on a data.frame counts columns

A query for no fields returns rows with zero columns, which `is_empty()` reports as empty. Use `NROW()` when you mean rows.

### `req_perform_parallel(on_error = "continue")` returns conditions

Failed pages come back as error objects in the response list, not responses. Passing them to a parser produces errors far from the cause. `check_resp_failures()` catches them.

### `...` forwards arbitrary Esri query parameters

`arc_select(..., returnDistinctValues = TRUE)` works because `...` goes straight to the API. Unknown names cannot be rejected outright, so `check_dots_query_names()` only flags names within two edits of a real argument.

## Formatting

`air format R/*.R` (`just fmt`). Run `devtools::document()` after changing roxygen.
