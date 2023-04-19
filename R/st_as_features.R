
#| This is distinctly different than a featureset
#| a featureset contains information about the crs the dimensions, etc
#| this is just an array of features
#| used for the /addFeatures endpoint
#| https://developers.arcgis.com/rest/services-reference/enterprise/add-features.htm
#|

# sfc objects to feature array json -----------------------------------------------------


#'
st_as_features <- function(x, ...) {
  UseMethod("st_as_features")
}



#' @export
st_as_features.sfc <- function(x, ...) {

  geoms <- featureset_geometry(x)

  res <- lapply(
    geoms[[1]],
    function(.x) c(list(attributes = c()), geometry = list(.x))
  )

  # cast to json
  jsonify::to_json(res, unbox = TRUE)
}

# sf objects --------------------------------------------------------------
#' @export
st_as_features.sf <- function(x, ...) {

  geo <- sf::st_geometry(x)
  geom_list <- featureset_geometry(geo)
  x <- sf::st_drop_geometry(x)

  # if there are no attributes
  if (nrow(x) == 0) {
    rows <- lapply(
      geoms_list[[1]],
      function(.x) c(list(attributes = c()), geometry = list(.x))
    )
  } else {
    # if attributes extract the fields
    rows <- mapply(
      function(.x, .y) c(list(attributes = c(.y)), geometry = list(.x)),
      geom_list[[1]],
      fields,
      SIMPLIFY = FALSE
    )

  }

  # cast to json
  jsonify::to_json(rows, unbox = TRUE)

}



# data.frame --------------------------------------------------------------
#' @export
st_as_features.data.frame <- function(x, ...) {

  # listify the fields
  fields <- transpose(x)

  # iterate over them and make them fit esri json format
  rows <- lapply(fields, function(.x) list(attributes = .x))

  # cast to json
  jsonify::to_json(rows, unbox = TRUE)

}

