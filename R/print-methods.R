# Table -------------------------------------------------------------------

#' @export
print.Table <- function(x, ...) {
  # create list of elements to print
  to_print <- compact(list(
    "Name" = x[["name"]],
    "Capabilities" = x[["capabilities"]],
    "Description" = x[["description"]]
  ))

  # filter out any 0 character strings
  print_index <- vapply(to_print, nzchar, logical(1))

  header <- sprintf(
    "<%s <%i features, %i fields>>",
    class(x), attr(x, "n"), length(x$fields$name)
  )

  # print only metadata that has values
  body <- paste0(
    names(to_print[print_index]),
    ": ",
    to_print[print_index]
  )

  # print the header and body
  cat(header, body, sep = "\n")

  # print the query if there is anything
  query <- compact(attr(x, "query"))

  if (any(lengths(query) > 0)) {

    # print if selection is made
    q_names <- names(query)

    # print the query if it exisrts
    q_str <- vapply(q_names, prettify_param, character(1), query, USE.NAMES = TRUE)
    q_body <- paste0(names(q_str), ": ", q_str)

    cat("Query:", q_body, sep = "\n  ")

  }

  invisible(x)

}

#' @export
head.Table <- function(x, n = 6, token = Sys.getenv("ARCGIS_TOKEN")) {
  collect_layer(x, n_max, token)
}

# Feature Layer -----------------------------------------------------------



# Print method for feature layer objects
#
#' @export
print.FeatureLayer <- function(x, ...) {

  to_print <- compact(list(
    "Name" = x[["name"]],
    "Geometry Type" = x[["geometryType"]],
    "CRS" = x[["extent"]][["spatialReference"]][["latestWkid"]],
    #"Query Formats" = x[["supportedQueryFormats"]],
    "Capabilities" = x[["capabilities"]]
  ))

  header <- sprintf(
    "<%s <%i features, %i fields>>",
    class(x), attr(x, "n"), length(x$fields$name)
  )

  body <- paste0(names(to_print), ": ", to_print)

  # cat out
  cat(header, body, sep = "\n")

  query <- compact(attr(x, "query"))

  if (any(lengths(query) > 0)) {
    # print if selection is made
    q_names <- names(query)

    q_str <- vapply(q_names, prettify_param, character(1), query, USE.NAMES = TRUE)
    q_body <- paste0(names(q_str), ": ", q_str)

    cat("Query:", q_body, sep = "\n  ")
  }

  invisible(x)
}


head.FeatureLayer <- function(x, n = 6, token = Sys.getenv("ARCGIS_TOKEN")) {
  collect_layer(x, n_max, token)
}


# Feature Server ----------------------------------------------------------

#' @export
print.FeatureServer <- function(x, n, ...) {

  n_fts <- length(x[["layers"]][["id"]])
  n_tbls <- length(x[["tables"]][["id"]])

  header <- sprintf(
    "<%s <%i layers>>",
    class(x), n_fts + n_tbls
  )


  # identify CRS
  crs <- x[["spatialReference"]][["latestWkid"]]

  # if this doesn't catch crs, then it will be NULL and omitted
  if (is.null(crs)) crs <- x[["fullExtent"]][["spatialReference"]][["latestWkid"]]

  # crs and capabilities will always be printed
  to_print <- compact(
    list(
      "CRS" = crs,
      "Capabilities" = x[["capabilities"]]
    )
  )

  body <- paste0(names(to_print), ": ", to_print)

  # extract layers
  lyr <- x[["layers"]]

  # LAYERS create and format vector to populate box

  box_layers_ln <- paste0(
    "  ",
    seq_len(nrow(lyr)),
    ": ",
    lyr[["name"]],
    " (",
    lyr[["geometryType"]],
    ")"
  )

  # Tables
  # if tables aren't missing populate
  tbls <- x[["tables"]]
  if (!is.null(tbls)) {

    box_tbl <- paste0(
      "  ",
      seq_len(nrow(tbls)),
      ": ",
      tbls[["name"]],
      " (",
      tbls[["geometryType"]],
      ")"
    )

  } else {
    # if missing assign to null
    box_tbl <- NULL
  }

  cat(header, body, box_layers_ln, box_tbl, sep = "\n")

  invisible(x)
}



# Image Server ------------------------------------------------------------

#' @export
print.ImageServer <- function(x, ...) {

  header <- sprintf(
    "<%s <%i bands, %i fields>>",
    class(x), x$bandCount, length(x$fields$name) %||% 0
  )

  extent <- paste(
    round(x[["extent"]][["xmin"]], 2),
    round(x[["extent"]][["xmax"]], 2),
    round(x[["extent"]][["ymin"]], 2),
    round(x[["extent"]][["ymax"]], 2),
    "(xmin, xmax, ymin, ymax)"
  )

  to_print <- compact(list(
    "Name" = x[["name"]],
    "Description" = substr(x[["description"]], 1, options('width')$width %||% 80 - 14),
    "Extent" = extent,
    "Resolution" = paste(round(x$pixelSizeX, 2), "x", round(x$pixelSizeY, 2)),
    "CRS" = x[["extent"]][["spatialReference"]][["latestWkid"]],
    "Capabilities" = x[["capabilities"]]
  ))

  body <- paste0(names(to_print), ": ", to_print)
  # cat out
  cat(header, body, sep = "\n")
  invisible(x)

}

# Utils -------------------------------------------------------------------

#' function to make printing easier
#'
#' The function takes a parameter name and the query list and trims each element
#' to fit on the contents of the window.
#'
#' @keywords internal
prettify_param <- function(param, query) {
  n_pad <- nchar(param) + 3
  cwidth <- options("width")[["width"]]
  width <- ifelse(is.null(cwidth), 20, cwidth)
  strtrim(as.character(query[[param]]), width - n_pad)
}
