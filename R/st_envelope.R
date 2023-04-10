
#' Return an envelope of a simple feature or simple feature set
#'
#' @details

#' An envelope is much like a bounding box. In fact, it is a bounding box when data
#' is only two dimensional. An envelope records the mimimum and maximum's of each
#' dimension. In 2D space this is `xmin`, `ymin`, `xmax`, and `ymax`. When a Z or M
#' dimension is present, it also records the min and maxes of those as well.
#'
#' @param x an object of class `sfg`, `sfc`, or `sf`
#'
#' @examples
#' st_envelope(sf::st_point())
#' @keywords internal
st_envelope <- function(x, crs) {
  UseMethod("st_envelope")
}

#'
#' @rdname st_envelope
#' @keywords internal
st_envelope.sfc <- function(x, crs = sf::st_crs(x)) {

  zrng <- attr(x, "z_range")
  mrng <- attr(x, "m_range")
  bbox <- st_bbox(x)

  new_envelope(c(bbox, mrng, zrng), crs = crs)

}


#'
#' @rdname st_envelope
#' @keywords internal
st_envelope.sf <- function(x, crs = sf::st_crs(x)) {
  st_envelope(sf::st_geometry(x))
}

#'
#' @rdname st_envelope
#' @keywords internal
st_envelope.sfg <- function(x, crs = NA) {

  x <- sf::st_sfc(x)
  st_envelope(x)

}


#'
#' @rdname st_envelope
#' @keywords internal
new_envelope <- function(x, crs) {
  n <- length(x)

  if (!(is.numeric(x) && (n %in% c(4, 6, 8)))) {
    stop(
      "`envelope` must be numeric and be of length 4, 6, or 8"
    )
  }

  nms <- c("xmin", "ymin", "xmax", "ymax", "zmin", "zmax", "mmin", "mmax")

  structure(x, names = nms[1:n], crs = crs, class = "envelope")
}


#'
#' @rdname st_envelope
#' @keywords internal
print.envelope <- function(x, digits = 4, ...) {
  print(round(structure(x, class = NULL, crs = NULL), digits))
  invisible(x)
}



# st_as_sfc ---------------------------------------------------------------

# https://stackoverflow.com/questions/25195363/draw-cube-vertices-with-fewest-number-of-steps


# st_as_sfc <- function(x, ...) UseMethod(x,...)
st_as_sfc.envelope <- function(x, ...) {

  if (!"zmin" %in% names(x)) {
    class(x) <- "bbox"
   return(sf::st_as_sfc(x))
  }

  vals <- c("xmin", "xmax", "xmax", "xmin", "xmin", "xmax", "xmax",
    "xmin", "ymin", "ymin", "ymax", "ymax", "ymin", "ymin", "ymax",
    "ymax", "zmin", "zmin", "zmin", "zmin", "zmax", "zmax", "zmax",
    "zmax")

  mat_str <- matrix(vals, ncol = 3, dimnames = list(NULL, c("x", "y", "z")))

  m <- matrix(x[mat_str], ncol = 3)


  sf::st_sfc(sf::st_polygon(list(rbind(m, m[1,]))), crs = sf::st_crs(x))

}
