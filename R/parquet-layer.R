#' Parquet Layers
#'
#' Upload a local parquet file to your portal as an `"Apache Parquet"` item,
#' or read an `"Apache Parquet"` item from your portal into R.
#'
#' @details
#'
#' `r lifecycle::badge("experimental")`
#'
#' - `upload_parquet()` adds a local parquet file as an `"Apache Parquet"` item
#'   in the content of the user associated with `token`. It is not published
#'   as a hosted feature layer.
#' - `read_parquet_layer()` reads an `"Apache Parquet"` item from your portal
#'   into R.
#'
#' At present, there is no support for partial reads of ParquetLayers.
#' Instead, the parquet file is downloaded into a temporary file and read using
#' the arrow R package. Reading GeoParquet files also requires geoarrow.
#'
#' @param file a length 1 character vector. The path to a `.parquet` file.
#' @param title a length 1 character vector. The title of the new item. Cannot
#'   be larger than 256 characters.
#' @param description optional. A length 1 character vector containing the
#'   description of the item. Cannot be larger than 64kb.
#' @param tags optional. A character vector of tags to add to the item.
#' @param snippet optional. A length 1 character vector with a short summary of
#'   the item. Cannot be longer than 2048 characters.
#' @param categories optional. A character vector of the categories of the item.
#' @param credits optional. A length 1 character vector crediting the source of
#'   the item. Sent as `accessInformation`.
#' @param license optional. A length 1 character vector describing the license
#'   or restrictions on the item. Sent as `licenseInfo`.
#' @param folder optional. The ID of the folder to add the item to. By default
#'   the item is added to the user's root folder.
#' @param item a `PortalItem` object, or the item ID or URL of a portal item.
#'   Must be an `"Apache Parquet"` item.
#' @inheritParams arcgisutils::refresh_token
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/add-item/)
#' @returns
#' - `upload_parquet()` returns a data frame with the columns `success`, `id`,
#'   and `folder` containing the ID of the newly created item.
#' - `read_parquet_layer()` returns the data frame read by
#'   [arrow::read_parquet()].
#' @rdname parquet
#' @export
#' @examplesIf curl::has_internet() && nzchar(Sys.getenv("NOT_CRAN"))
#' library(geoarrow)
#' library(arcgisutils)
#'
#' set_arc_token(auth_user())
#'
#' nc <- sf::read_sf(system.file("gpkg/nc.gpkg", package = "sf"))
#' tf <- tempfile(fileext = ".parquet")
#' arrow::write_parquet(as.data.frame(nc), tf)
#'
#' res <- upload_parquet(
#'   tf,
#'   "North Carolina SIDS",
#'   tags = c("north carolina", "sids"),
#'   snippet = "SIDS cases in North Carolina counties"
#' )
#'
#' read_parquet_layer(res$id)
upload_parquet <- function(
  file,
  title,
  description = NULL,
  tags = NULL,
  snippet = NULL,
  categories = NULL,
  credits = NULL,
  license = NULL,
  folder = NULL,
  token = arc_token()
) {
  obj_check_token(token)
  check_token_has_user(token)

  check_string(file, allow_empty = FALSE)
  check_string(title, allow_empty = FALSE)
  check_string(description, allow_null = TRUE)
  check_character(tags, allow_null = TRUE)
  check_string(snippet, allow_null = TRUE)
  check_character(categories, allow_null = TRUE)
  check_string(credits, allow_null = TRUE)
  check_string(license, allow_null = TRUE)
  check_string(folder, allow_null = TRUE, allow_empty = FALSE)

  if (!file.exists(file)) {
    cli::cli_abort("{.arg file} does not exist: {.file {file}}")
  }

  user <- token[["username"]]
  host <- token[["arcgis_host"]]

  req_url <- paste(
    c(host, "sharing/rest/content/users", user, folder, "addItem"),
    collapse = "/"
  )

  body <- compact(list(
    f = "json",
    type = "Apache Parquet",
    title = title,
    description = description,
    tags = if (!is.null(tags)) paste0(tags, collapse = ","),
    snippet = snippet,
    categories = if (!is.null(categories)) paste0(categories, collapse = ","),
    accessInformation = credits,
    licenseInfo = license,
    file = curl::form_file(file)
  ))

  req <- arc_base_req(req_url, token = token) |>
    httr2::req_body_multipart(!!!body)

  resp <- httr2::req_perform(req)
  parsed <- RcppSimdJson::fparse(httr2::resp_body_string(resp))
  detect_errors(parsed)
  data_frame(as.data.frame(parsed))
}

#' @rdname parquet
#' @export
read_parquet_layer <- function(item, token = arc_token()) {
  rlang::check_installed("arrow", "to read parquet files.")
  if (!requireNamespace("geoarrow", quietly = TRUE)) {
    cli::cli_alert_warning(
      "{.pkg geoarrow} is needed to read GeoParquet files."
    )
  }

  if (!(inherits(item, "PortalItem") || rlang::is_string(item))) {
    cli::cli_abort("{.arg item} must be a string or {.cls PortalItem}")
  }

  if (rlang::is_string(item)) {
    item <- arc_open(item, token = token)
  }

  if (!item$type == "Apache Parquet") {
    cli::cli_abort(
      "Expected {.code Apache Parquet} item type. Found {.code {item$type}} instead."
    )
  }

  bytes <- arc_item_data(item, token = token)
  tmp <- tempfile(fileext = ".parquet")
  writeBin(bytes, tmp)
  arrow::read_parquet(tmp)
}
