#' @export
#' @inheritParams add_features
#' @rdname modify
update_features <- function(
  x,
  .data,
  chunk_size = 1000,
  match_on = c("name", "alias"),
  rollback_on_failure = TRUE,
  progress = TRUE,
  token = arc_token()
) {
  match_on <- match.arg(match_on)

  # TODO field types from x need to be compared to that of `.data`
  # if the field types do not match either error or do the conversion for the user

  # What is the difference between `applyEdits`, and `append` both are a way to do upserting.

  # Update Feature Layer
  # https://developers.arcgis.com/rest/services-reference/enterprise/update-features.htm
  # check CRS compaitibility between x and `.data`
  # feedback for rest api team:
  # it is possible to specify the CRS of the input data for adding features or spatial
  # filters, however it is _not_ possible for updating features. If it is, it is undocumented
  # it would be nice to be able to utilize the the transformations server side
  # rather than relying on GDAL client side.
  # ALTERNATIVELY let me provide a feature set so i can pass in CRS
  if (!identical(sf::st_crs(x), sf::st_crs(.data))) {
    if (is.na(sf::st_crs(.data)) && inherits(.data, "sf")) {
      cli::cli_warn(
        c(
          "{.arg data} is missing a CRS",
          "i" = paste0("Setting CRS to ", sf::st_crs(x)$srid)
        )
      )
    } else if (inherits(.data, "sf")) {
      cli::cli_abort(
        c(
          "{.arg x} and {.arg .data} must share the same CRS",
          "*" = "Tranform {.arg .data} to the same CRS as {.arg x} with
          {.fn sf::st_transform}"
        )
      )
    }
  } # not that addFeatures does not update layer definitions so if any attributes
  # are provided that aren't in the feature layer, they will be ignored

  cnames <- colnames(.data)
  feature_fields <- list_fields(x)

  # find which columns are present in the layer
  present_index <- cnames %in% feature_fields[[match_on]]

  oid_idx <- which(feature_fields[["type"]] == "esriFieldTypeOID")

  # get the column name for the object ID
  oid_col_name <- feature_fields[[match_on]][oid_idx]

  # extract the oid column
  oid_col_data <- .data[[oid_col_name]]

  if (rlang::is_null(oid_col_data)) {
    cli::cli_abort(
      c(
        "Unable to find Object ID field in {.arg .data}.",
        "i" = "The Object ID is required to identify which features to update."
      )
    )
  }

  if (rlang::is_integerish(oid_col_data)) {
    .data[[oid_col_name]] <- as.integer(oid_col_data)
  } else {
    cli::cli_abort(
      c(
        "x" = "The {.field objectid} column must be of type {.cls integer}.",
        "i" = "Convert with {.code as.integer()} if needed."
      )
    )
  }

  # fetch the geometry column name
  geo_col <- attr(.data, "sf_column")

  if (match_on == "alias") {
    lu <- stats::setNames(feature_fields[["name"]], feature_fields[["alias"]])

    # ensure the geo_col is present if its an sf object
    if (inherits(.data, "sf")) {
      cnames[length(cnames)] <- geo_col
    } else {
      cnames <- unname(lu[cnames])
    }
    colnames(.data) <- cnames
  }

  inform_nin_feature(
    # columns not in the feature layer
    setdiff(cnames[!present_index], geo_col)
  )

  # subset accordingly
  .data <- .data[, present_index]

  # count the number of rows in a data frame
  n <- nrow(.data)

  # get the indices to chunk them up
  indices <- chunk_indices(n, chunk_size)

  # create base request
  req <- arc_base_req(paste0(x[["url"]], "/updateFeatures"), token)

  # # pre-allocate list
  all_reqs <- vector("list", length = lengths(indices)[1])

  # # populate the requests veector
  for (i in seq_along(all_reqs)) {
    start <- indices[["start"]][i]
    end <- indices[["end"]][i]

    all_reqs[[i]] <- httr2::req_body_form(
      req,
      features = as_esri_features(.data[start:end, ]),
      rollbackOnFailure = rollback_on_failure,
      f = "json"
    )
  }

  # send the requests in parallel
  all_resps <- httr2::req_perform_parallel(all_reqs, progress = progress)

  # parse the responses into a data frame
  res <- do.call(
    rbind.data.frame,
    lapply(all_resps, function(res) {
      resp_str <- httr2::resp_body_string(res)
      catch_error(resp_str)
      resp <- RcppSimdJson::fparse(resp_str)
      resp[["updateResults"]]
    })
  )
  data_frame(res)
}
