
#| This is distinctly different than a featureset
#| a featureset contains information about the crs the dimensions, etc
#| this is just an array of features
#| used for the /addFeatures endpoint
#| https://developers.arcgis.com/rest/services-reference/enterprise/add-features.htm
#|

# sfc objects to feature array json -----------------------------------------------------

#' @param ... unused
#' @export
#' @rdname st_as_json
st_as_features <- function(x, ...) {
  UseMethod("st_as_features")
}



#' @export
st_as_features.sfc <- function(x, ...) {

  geoms <- featureset_geometry(x)

  res <- purrr::map(
    geoms[[1]],
    ~c(list(attributes = c()), geometry = list(.x))
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

  fields <- transpose(x)

  if (length(x) == 0) {
    rows <- purrr::map(
      geom_list[[1]],
      ~c(list(attributes = c()), geometry = list(.x))
    )

  } else {

    rows <- purrr::map2(
      fields,
      geom_list[[1]],
      ~c(list(attributes = .x, geometry = .y))
    )

  }

  # cast to json
  jsonify::to_json(rows, unbox = TRUE)

}



# data.frame --------------------------------------------------------------
#' @export
st_as_features.data.frame <- function(x, ...) {
  fields <- transpose(x)

  rows <- purrr::map(fields, ~list(attributes = .x))

  # cast to json
  jsonify::to_json(rows, unbox = TRUE)

}

