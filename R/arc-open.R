#' Open connection to remote resource
#'
#' Provided a URL, create an object referencing the remote resource.
#' The resultant object acts as a reference to the remote data source.
#'
#' To extract data from the remote resource use [`arc_select()`] for objects of
#' class `FeatureLayer` or `Table`. For `ImageServer`s, use [`arc_raster()`].
#'
#'  `r lifecycle::badge("experimental")`
#'
#' @param url The url of the remote resource. Must be of length one.
#' @param token your authorization token.
#'
#' @seealso arc_select arc_raster
#' @export
#' @returns
#' Depending on the provided URL returns a `FeatureLayer`, `Table`,
#' `FeatureServer`, `ImageServer`, `MapServer`, `GroupLayer`, `FeatureService`,
#' or `MapService`. Each of these objects is a named list containing the
#' properties of the service.
#' @examples
#' \dontrun{
#' # FeatureLayer
#' furl <- paste0(
#'   "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/",
#'   "PLACES_LocalData_for_BetterHealth/FeatureServer/0"
#' )
#'
#' arc_open(furl)
#'
#' # Table
#' furl <- paste0(
#'   "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/",
#'   "USA_Wetlands/FeatureServer/1"
#' )
#'
#' arc_open(furl)
#'
#' # ImageServer
#' arc_open(
#'   "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"
#' )
#'
#' # FeatureServer
#' furl <- paste0(
#'   "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/",
#'   "PLACES_LocalData_for_BetterHealth/FeatureServer"
#' )
#'
#' arc_open(furl)
#'
#' # MapServer
#' map_url <- paste0(
#'   "https://services.arcgisonline.com/ArcGIS/rest/services/",
#'   "World_Imagery/MapServer"
#' )
#'
#' arc_open(map_url)
#'
#' # Item Page (Feature Service or Map Service)
#'
#' item_url <- paste0(
#'   "https://www.arcgis.com/home/item.html",
#'   "?id=10df2279f9684e4a9f6a7f08febac2a9"
#' )
#'
#' arc_open(item_url)
#'
#' # Group Page (Feature Service)
#'
#' group_url <- paste0(
#'   "https://www.arcgis.com/home/group.html",
#'   "?id=1d1f24e8556642f49448f1c88b5a571b"
#' )
#'
#' arc_open(group_url)
#'
#' }
arc_open <- function(url, token = arc_token()) {
  check_url(url)

  content_url <- NULL

  # test if URL is a content url (one of "search", "item", "group", or "user")
  # if so, parse the public-facing content url and build a content API url
  if (is_arc_content_url(url)) {
    type <- arc_content_url_type(url)
    stopifnot(type %in% c("item", "group"))
    content_url <- url
    url <- arc_url_item(url, key = "id", path = paste0(type, "s"))
  }

  # parse url query and strip from url if query matches default
  query <- parse_url_query(url) %||% list()
  url <- clear_url_query(url)

  # extract layer metadata
  meta <- fetch_layer_metadata(url, token)

  # set url for later use
  meta[["url"]] <- url
  meta[["content_url"]] <- content_url

  # layer class
  layer_class <- gsub("\\s", "", meta[["type"]])

  # if it's missing it means it's a server type. Need to deduce.
  if (length(layer_class) == 0) {
    if (any(grepl("pixel|band|raster", names(meta)))) {
      layer_class <- "ImageServer"
    } else if (grepl("MapServer", meta[["url"]])) {
      layer_class <- "MapServer"
    } else if ("layers" %in% names(meta) || grepl("FeatureServer", meta[["url"]])) {
      layer_class <- "FeatureServer"
    } else {
      # FIXME: Group URLs end up on this path
      return(meta)
    }
  }

  # construct the appropriate class based on the resultant `layer_class`
  res <- switch(layer_class,
    "FeatureLayer" = structure(
      meta,
      class = layer_class,
      query = query
    ),
    "Table" = structure(
      meta,
      class = layer_class,
      query = query
    ),
    "FeatureServer" = structure(
      meta,
      class = layer_class
    ),
    "ImageServer" = structure(meta, class = layer_class),
    "MapServer" = structure(meta, class = layer_class),
    "GroupLayer" = structure(meta, class = layer_class),
    "FeatureService" = structure(meta, class = layer_class),
    "MapService" = structure(meta, class = layer_class),
    cli::cli_abort(
      c(
        "Service type {.val {layer_class}} is not supported.",
        "i" = "Please report this at {.url https://github.com/R-ArcGIS/arcgislayers/issues}"
      )
    )
  )

  res
}

#' Test if the vector is a content url of the supplied type
#' @keywords internal
is_arc_content_url <- function(x, type = c("search", "item", "group", "user")) {
  pattern <- paste0("arcgis\\.com/home/(", paste0(type, collapse = "|"), ")\\.html")
  is.character(x) & grepl(pattern, x, perl = TRUE)
}

#' Convert a URL into a content URL
#' @keywords internal
arc_url_item <- function(
    id,
    key = "id",
    base_url = "https://www.arcgis.com/sharing/rest/content",
    path = "items",
    call = caller_env()) {
  if (is_url(id)) {
    id <- httr2::url_parse(id)[["query"]][[key]]
  }

  check_string(id, call = call)

  paste(base_url, path, id, sep = "/")
}

#' Extract the content URL type
#' @noRd
arc_content_url_type <- function(url) {
  str_extract(url, "(?<=arcgis\\.com/home/)[[:lower:]]+(?=\\.html)")
}

#' Copied from stringstatic - 2024-10-31
#' https://github.com/rossellhayes/stringstatic/blob/main/R/str_extract.R
#' @noRd
str_extract <- function(string, pattern) {
  if (length(string) == 0 || length(pattern) == 0) return(character(0))

  is_fixed <- inherits(pattern, "stringr_fixed")

  result <- Map(
    function(string, pattern) {
      if (is.na(string) || is.na(pattern)) return(NA_character_)

      regmatches(
        x = string,
        m = regexpr(
          pattern = pattern, text = string, perl = !is_fixed, fixed = is_fixed
        )
      )
    },
    string, pattern, USE.NAMES = FALSE
  )

  result[lengths(result) == 0] <- NA_character_
  unlist(result)
}
