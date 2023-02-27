# url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/List_of_Providers/FeatureServer/27"
#

#' Feature Table
#'
#' @export
#' @examples
#' tbl_url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/List_of_Providers/FeatureServer/27"
#'
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
  # always printed
  cli::cli_text(
    cli::style_bold("<", paste(class(x), collapse = "/"), " <"),

    cli::style_bold(
      cli::style_italic(
        "{attr(x,'n')} featur{?e/es}, {length(x$fields$name)} field{?/s}"
      )),
    cli::style_bold(">>")
  )

  # create list of elements to print
  to_print <- compact(list(
    "Name" = x[["name"]],
    "Capabilities" = x[["capabilities"]],
    "Description" = x[["description"]]
  ))

  # filter out any 0 character strings
  print_index <- vapply(to_print, nzchar, logical(1))

  cli::cli_dl(to_print[print_index])


  # print the query if there is anything
  query <- compact(attr(x, "query"))

  if (any(lengths(query) > 0)) {
    cli::cli_h3("Query")
    # print if selection is made
    q_names <- names(query)

    cli::cli_dl(
      setNames(
        lapply(q_names, prettify_param, query),
        q_names
      )
    )

  }

  invisible(x)

}

#' dplyr methods
#'
#' @rdname dplyr
collect.Table <- function(x, token = "", ...) {
  # stop if query not supported
  if (!grepl("query", x[["capabilities"]], ignore.case = TRUE)) {
    cli::cli_abort("Table {.val {x[['name']]}} does not support querying")
  }

  # extract existing query
  query <- attr(x, "query")

  # parameter validation ----------------------------------------------------
  # get existing parameters
  # manually change f = pgeojson to f = json since this function is intended
  # for use with feature layers
  query_params <- validate_params(query, token = token)

  query_params[["f"]] <- "pjson"


  # Offsets -----------------------------------------------------------------
  feats_per_page <- x[["maxRecordCount"]]

  # set the basic request
  req <- httr2::request(x[["url"]])

  # count the number of features in a query
  n_req <-
    httr2::req_url_query(
      httr2::req_url_query(
        httr2::req_url_path_append(req, "query"),
        !!!query_params
      ),
      returnCountOnly = "true"
    )

  # make request for total number of features based on query
  n_feats <- httr2::resp_body_json(
    httr2::req_perform(
      httr2::req_url_query(n_req, f = "pjson")
    ), check_type = FALSE
  )[["count"]]

  # identify the number of pages needed to return all features
  n_pages <- floor(n_feats / feats_per_page)

  # identify the offsets needed to get all pages
  # if n_pages is 0 we set offsets to 0 straight away
  if (n_pages == 0) {
    offsets <- 0
  } else {
    offsets = c(0, (feats_per_page * 1:n_pages) + 1)
  }

  # create a list of requests
  all_requests <- lapply(offsets, add_offset, req, query_params)

  # make all requests and store responses in list
  all_resps <- httr2::multi_req_perform(all_requests)

  # identify any errors
  has_error <- vapply(all_resps, function(x) inherits(x, "error"), logical(1))

  if (any(has_error)) {
    cli::cli_warn("{.val {sum(has_error)}} page of responses unsuccessfully retrieved")
  }

  # fetch the results
  res <- lapply(
    all_resps[!has_error],
    function(x) {
      # parse the json
      res_list <- RcppSimdJson::fparse(
        httr2::resp_body_string(x),
        empty_array = list(),
        empty_object = NA,
        single_null = NA
      )[["features"]][["attributes"]] # grab only important fiedls

      # force into data.frame
      do.call(rbind.data.frame, res_list)
      }
  )

  # combine all results into tibble
  do.call(tibble::tibble, res)
}
