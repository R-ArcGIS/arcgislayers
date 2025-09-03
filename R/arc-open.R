#' Access a Data Service or Portal Item
#'
#' Access a resource on ArcGIS Online, Enterprise, or Location Platform.
#'
#' @details

#' - To read the underlying attribute data from a `FeatureLayer`, `Table`, or `ImageServer` use [`arc_select()`].
#' - If you have a `MapServer` or `FeatureSever` access the individual layes using [`get_layer()`]. For
#' - Use [`arc_raster()`] to get imagery as a terra raster object.
#'
#'  `r lifecycle::badge("stable")`
#'
#' @param url a url to a service such as a feature service, image server, or map server. Alternatively, an item ID of a portal item or portal url.
#' @inheritParams arcgisutils::arc_item
#'
#' @seealso arc_select arc_raster get_layer
#' @export
#' @returns
#' Depending on item ID or URL returns a `PortalItem`, `FeatureLayer`, `Table`, `FeatureServer`, `ImageServer`, or `MapServer`, `GeocodeServer`, among other. Each of these objects is a named list containing the properties of the service.
#' @examples
#' \dontrun{
#'
#' # FeatureServer ID
#' arc_open("3b7221d4e47740cab9235b839fa55cd7")
#'
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
#' }
arc_open <- function(url, host = arc_host(), token = arc_token()) {
  check_string(url, allow_empty = FALSE)

  if (!is_url(url)) {
    e_msg <- "Expected an item ID or url to a portal item."

    item <- rlang::try_fetch(
      arc_item(url, host, token),
      error = function(cnd) {
        cli::cli_abort(
          c(e_msg, cnd$message),
          call = rlang::caller_call(2),
          trace = cnd$trace
        )
      }
    )

    if (is.null(item$url)) {
      # return a portal item if the url is null
      return(item)
    }

    # return the portal item if the url type is null
    if (is.null(arc_url_type(item$url))) {
      return(item)
    }

    # otherwise we fetch the url from the new item
    url <- URLencode(item$url)
  }

  # parse the provided url
  info <- arc_url_parse(url)

  if (is.null(info$type)) {
    cli::cli_abort(
      c(
        "!" = "Unable to open the provided url or item ID.",
        "i" = "If you think this an error, please create an issue:",
        "{.url https://github.com/r-arcgis/arcgislayers/issues/new}"
      )
    )
  }

  # get the first element if since it can have more than one type
  # for service folders
  layer_type <- info$type[1]
  switch(
    layer_type,
    "FeatureServer" = {
      as_layer_class(clear_url_query(url), token, layer_type)
    },
    "MapServer" = as_layer_class(
      clear_url_query(url),
      token,
      layer_type
    ),
    "ImageServer" = as_layer_class(
      clear_url_query(url),
      token,
      layer_type
    ),
    "SceneServer" = as_layer_class(url, token, layer_type),
    "GeocodeServer" = as_layer_class(url, token, layer_type),
    # FIXME, unclear how to use this...
    "GeometryServer" = as_layer_class(url, token, layer_type),
    # FIXME, unclear how to use this...
    "GPServer" = as_layer_class(url, token, layer_type),
    "item" = {
      # if we have an item url, we fetch the item
      item <- arc_item(info$query$id, host = host, token = token)

      # if there is no associated url we return the item
      if (is.null(item[["url"]])) {
        return(item)
      }

      url_type <- arc_url_type(item[["url"]])

      if (is.null(url_type)) {
        return(item)
      }

      # if there is a URL we're going to recurse
      arc_open(URLencode(item[["url"]]), host = host, token = token)
    },
    "user" = arc_user(info$query$user, host = host, token = token),
    "group" = arc_group(info$query$id, host = host, token = token),
    "webscene" = arc_item(info$query$webscene, host = host, token = token),
    "app" = arc_item(info$query$appid, host = host, token = token),
    "notebook" = arc_item(info$query$id, host = host, token = token),
    "experience" = {
      path_components <- strsplit(info$path, "/")[[1]]
      exp_id <- path_components[which(path_components == "experience") + 1]
      arc_item(exp_id, host = host, token = token)
    },
    "storymap" = {
      path_components <- strsplit(info$path, "/")[[1]]
      sm_id <- path_components[which(path_components == "stories") + 1]
      arc_item(sm_id, host = host, token = token)
    },
    "dashboard" = {
      path_components <- strsplit(info$path, "/")[[1]]
      db_id <- path_components[which(path_components == "dashboards") + 1]
      arc_item(db_id, host = host, token = token)
    },
    "datapipeline" = arc_item(info$query$item, host = host, token = token),
    "webapp" = arc_item(info$query$id, host = host, token = token),
    "service_folder" = as_layer_class(url, token = token),
    cli::cli_abort(
      c(
        "Service type {.val {layer_type}} is not supported at this time.",
        "i" = "Please report this at {.url https://github.com/R-ArcGIS/arcgislayers/issues}"
      )
    )
  )
}

#' Fetch metadata and an appropriate class
#' @noRd
as_layer_class <- function(url, token, class = NULL) {
  meta <- fetch_layer_metadata(url, token)
  meta[["url"]] <- url
  cls <- if (is.null(meta[["type"]])) {
    NULL
  } else {
    gsub("\\s+", "", meta[["type"]])
  }

  structure(meta, class = c(cls %||% class, "list"))
}
