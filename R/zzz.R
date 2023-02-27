.onLoad <- function(...) {
  # register_all_s3_methods()
  requireNamespace("dplyr")
  register_all_s3_methods()
  invisible()
}


# from: https://raw.githubusercontent.com/r-spatial/sf/main/R/tidyverse.R
# 2022-06-27 08:54:11

register_all_s3_methods = function() {
  register_s3_method("sf", "st_as_sfc", "envelope")
  register_s3_method("sf", "st_crs", "FeatureLayer")
  register_s3_method("dplyr", "collect", "FeatureLayer")
  register_s3_method("dplyr", "collect", "Table")
  register_s3_method("dplyr", "filter", "Table")
  register_s3_method("dplyr", "filter", "FeatureLayer")
  register_s3_method("dplyr", "select", "FeatureLayer")
  register_s3_method("dplyr", "select", "Table")


}




# from: https://github.com/tidyverse/hms/blob/master/R/zzz.R
# Thu Apr 19 10:53:24 CEST 2018
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
