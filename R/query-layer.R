#' Retrieve a feature layer
#'
#' Give a `FeatureLayer` or `Table` object, retrieve its data as an `sf` object or `tibble` resepctively.
#'
#' @param x an object of class `FeatureLayer` or `Table`.
#' @param fields a character vector of the field names that you wish to be returned. By default all fields are returned.
#' @param crs the spatial reference to be returned. If the CRS is different than the `FeatureLayer`'s CRS, a transformation will occur server-side. Ignored for `Table` objects.
#' @param filter_geom an object of class `sfc` or `sfg` used to filter query results based on a predicate function. If an `sfc` object is provided it will be transformed to the layers spatial reference. If the `sfc` is missing a CRS (or is an `sfg` object) it is assumed to be in the layers spatial reference.
#' @param predicate default `"intersects"`. Possible options are `"intersects"`,  `"contains"`,  `"crosses"`,  `"overlaps"`,  `"touches"`, and `"within"`.
#' @param n_max the maximum number of features to return. By default returns every feature available. Unused at the moment.
#' @param ... additional query parameters passed to `update_params()`. See [reference documentation](https://developers.arcgis.com/rest/services-reference/enterprise/query-feature-service-layer-.htm#GUID-BC2AD141-3386-49FB-AA09-FF341145F614) for possible arguments.
#' @export
query_layer <- function(
    x,
    fields,
    where,
    crs = sf::st_crs(x),
    filter_geom,
    predicate = "intersects",
    n_max = Inf,
    ...
) {


  if (!missing(filter_geom)) {

    # if its an sfc object it must be length one
    if (inherits(filter_geom, "sfc") && length(filter_geom) > 1) {
      stop("`filter` must be a single geometry")
    }


    if (inherits(filter_geom, "sfc")) {
      # if the CRS is missing use the layer's CRS
      # otherwise use the CRS of the filter_geom object
      filt_crs <- sf::st_crs(filter_geom)

      if (is.na(filt_crs)) {
        filt_crs <- crs
      }
      # extract the sfg object which is used to write Esri json
      filter_geom <- filter_geom[[1]]
    }
    # if a multi polygon stop, must be a single polygon see
    # related issue: https://github.com/R-ArcGIS/api-interface/issues/4
    if (inherits(filter_geom, "MULTIPOLYGON")) stop("`filter_geom` cannot be a MULTIPOLYGON")


    esri_geometry <- st_as_json(filter_geom, filt_crs)

    filter_params <- list(
      geometryType = determine_esri_geo_type(filter_geom),
      geometry = st_as_json(filter_geom),
      spatialRel = match_spatial_rel(predicate)
      # TODO is `inSR` needed if the CRS is specified in the geometry???
    )

    x <- update_params(x, rlang::splice(filter_params))

  }

  query <- attr(x, "query")

  # handle fields and where clause if missing
  if (missing(fields)) {
    fields <- query[["outFields"]] %||% "*"
  }

  # if not missing fields collapse to scalar character
  if (!missing(fields) && (length(fields) > 1)) {
    # check if incorrect field names provided
    x_fields <- x[["fields"]][["name"]]
    nindex <- tolower(fields) %in% tolower(x_fields)

    if (any(!nindex)) {

      stop(
        "Field(s) not in `x`:\n  ",
        paste(fields[!nindex], collapse = ", ")
      )

    }

    fields <- paste0(fields, collapse = ",")
  }

  # if where is missing set to 1=1
  if (missing(where)) {
    where <- query[["where"]] %||% "1=1"
  }


  x <- update_params(
    x,
    outFields = fields,
    where = where,
    outSR = jsonify::to_json(validate_crs(crs)[[1]], unbox = TRUE),
    ...
  )

  # return(x)
  collect_layer(x, n_max = n_max)
}

