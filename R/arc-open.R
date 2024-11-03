#' Open connection to remote resource
#'
#' [arc_open()] uses a URL to create an object referencing the remote resource.
#' Typically, `arc_open` is used with a FeatureServer, ImageServer, or
#' MapServer URL and uses the [ArcGIS Server Services Directory
#' API](https://developers.arcgis.com/rest/services-reference/enterprise/get-started-with-the-services-directory/).
#' If an item or group URL is supplied, `arc_open` calls [arc_sharing()] which
#' uses the [ArcGIS Portal Directory REST
#' API](https://developers.arcgis.com/rest/users-groups-and-items/working-with-users-groups-and-items/)
#' (also known as the Sharing API) for working with users, groups, and content
#' in an organization.
#'
#' To extract data from the remote resource use [`arc_select()`] for objects of
#' class `FeatureLayer` or `Table`. For `ImageServer`s, use [`arc_raster()`].
#'
#'  `r lifecycle::badge("experimental")`
#'
#' @param url The url of the remote resource. Must be of length one.
#' @param token your authorization token.
#' @param ... Additional parameters passed by [arc_open()] to [arc_sharing()].
#' @seealso [arc_select()]; [arc_raster()]
#' @export
#' @returns
#' Depending on the provided URL, `arc_open()` returns a `FeatureLayer`,
#' `Table`, `FeatureServer`, `ImageServer`, `MapServer`, `GroupLayer`,
#' `FeatureService`, or `MapService`. `arc_sharing()` returns a `ArcGIS item`,
#' `ArcGIS user`, or `ArcGIS group` object. Each of these objects is a named
#' list containing the properties of the service.
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
#' # Item Page
#' item_url <- paste0(
#'   "https://www.arcgis.com/home/item.html",
#'   "?id=58e5b2fe5aaa4aa782175c334734e0a9"
#' )
#'
#' arc_open(item_url)
#'
#' # Group Page
#' group_url <- paste0(
#'   "https://www.arcgis.com/home/group.html",
#'   "?id=c755678be14e4a0984af36a15f5b643e"
#' )
#'
#' arc_open(group_url)
#'
#' # Sharing API
#' sharing_api_url <- paste0(
#'   "https://www.arcgis.com/sharing/rest",
#'   "/community/groups/1d1f24e8556642f49448f1c88b5a571b"
#' )
#'
#' arc_open(sharing_api_url)
#'
#' }
arc_open <- function(url, token = arc_token(), ...) {
  check_url(url)

  # test if URL is a content url
  # TODO: If an item URL is a reference to a FeatureLayer, Table, FeatureServer,
  # etc., provide the option to use the item metadata to overwrite the URL and
  # skip this early return
  if (is_arc_content_url(url) || grepl("/sharing/rest/", url)) {
    return(arc_sharing(url, token = token, ...))
  }

  # parse url query and strip from url if query matches default
  query <- parse_url_query(url) %||% list()
  url <- clear_url_query(url)

  # extract layer metadata
  meta <- fetch_layer_metadata(url, token)

  # set url for later use
  meta[["url"]] <- url

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
    cli::cli_abort(
      c(
        "Service type {.val {layer_class}} is not supported.",
        "i" = "Please report this at
        {.url https://github.com/R-ArcGIS/arcgislayers/issues}"
      )
    )
  )

  res
}

#' [arc_sharing()] supports both ArcGIS Portal API request URLs and content URLs
#' for item, user, or group pages on arcgis.com.
#' @param resource Resource type to access with the ArcGIS Portal API. One of
#'   "content" (default), "community", or "portals".
#' @inheritParams rlang::args_error_context
#' @rdname arc_open
#' @export
arc_sharing <- function(url,
                        token = arc_token(),
                        resource = c("content", "community", "portals"),
                        call = caller_env()) {
  # TODO: Improve URL validation
  check_url(url, pattern = "/sharing/rest/|\\.html", call = call)

  if (grepl("/sharing/rest/", url)) {
    request_url <- url
    # FIXME: type could still be inferred from the API url
    type <- "item"
  } else {
    resource <- rlang::arg_match(resource, error_call = call)

    query <- parse_url_query(url)

    # Extract the URL type - one of "search", "item", "group", or "user"
    type <- extract_content_url_type(url)

    # Set the key based on the URL type
    key <- switch(type,
                  item = "id",
                  group = "id",
                  user = "user"
    )

    # Building a Portal API request URL
    # TODO: Improve the flexibility of this URL building step to better support
    # other API endpoints
    request_url <- arc_sharing_url_build(
      append = paste0("/", type, "s", "/", query[[key]]),
      resource = resource,
      call = call
    )
  }

  # FIXME: `fetch_layer_metadata()` should allow additional query parameters
  meta <- fetch_layer_metadata(request_url, token, call = call)

  structure(
    meta,
    # Set class to ArcGIS item, ArcGIS user, ArcGIS group, etc.
    class = paste0("ArcGIS ", type),
    # NOTE: Unlike `arc_open`, class should not be pulled from metadata "type"
    # element as groups do not have a type and type includes a wide variety
    # (e.g. PDF, Code Sample, Notebook)

    # Include both input URL and request URL
    url = url,
    request_url = request_url
  )
}

#' Convert a URL into a Sharing API URL
#' @noRd
arc_sharing_url_build <- function(
    append = "/content/items",
    resource = c("content", "community", "portals"),
    host = arc_host(),
    call = caller_env()) {
  # TODO: Add a check if host includes an existing path
  # TODO: Add a check for /webadaptor/ suffix when host != arcgis.com
  url <- httr2::url_parse(host)

  resource <- rlang::arg_match(resource, error_call = call)

  # Extend path with append and id values
  url[["path"]] <- paste0("/sharing/rest/", resource, append)

  httr2::url_build(url)
}
