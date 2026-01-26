# List Available Raster Funcitons

This function returns the `rasterFunctionInfos` field of the
`ImageServer`'s metadata as a `data.frame`. If the field does not exist
then an error is emitted.

## Usage

``` r
list_raster_fns(x, arg = rlang::caller_arg(x), call = rlang::caller_call())

list_service_raster_fns(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_call()
)
```

## Arguments

- x:

  an `ImageServer`.

- arg:

  An argument name in the current function.

- call:

  The execution environment of a currently running function, e.g.
  `call = caller_env()`. The corresponding function call is retrieved
  and mentioned in error messages as the source of the error.

  You only need to supply `call` when throwing a condition from a helper
  function which wouldn't be relevant to mention in the message.

  Can also be `NULL` or a [defused function
  call](https://rlang.r-lib.org/reference/topic-defuse.html) to
  respectively not display any call or hard-code a code to display.

  For more information about error calls, see [Including function calls
  in error
  messages](https://rlang.r-lib.org/reference/topic-error-call.html).

## Value

a data.frame of the available raster functions.

## Examples

``` r
if (FALSE) { # \dontrun{
# use paste to avoid cran note
furl <- paste0(
  "https://di-usfsdata.img.arcgis.com/arcgis/rest/services",
  "/FIA_BIGMAP_2018_Tree_Species_Aboveground_Biomass/ImageServer"
)

service <- arc_open(furl)
raster_fns <- list_service_raster_fns(service)
head(raster_fns)
} # }
```
