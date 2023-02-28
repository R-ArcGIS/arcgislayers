# url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/List_of_Providers/FeatureServer/27"
#

#' Feature Table
#'
#' @export
#' @examples
#' tbl_url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/List_of_Providers/FeatureServer/27"
#' feature_table(tbl_url)
feature_table <- function(url, token = "") {

  req <- httr2::request(url)
  meta <- fetch_layer_metadata(req, token = token)
  n_feats <- count_features(req, token)
  meta <- compact(meta)
  meta[["url"]] <- url

  if (meta[["type"]] != "Table") {
    cli::cli_abort(
      c("url is not a Table",
        "i" = "{.val {meta[['type']]}} provided"))
  }

  structure(
    meta,
    class = "Table",
    n = n_feats,
    query = list()
  )
}



#' @exportS3Method
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

