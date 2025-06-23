check_query_value <- function(
  x,
  what,
  ...,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  # Allow non-empty strings
  if (rlang::is_string(x) && x != "") {
    return(invisible(NULL))
  }

  # Allow scalar logical values
  if (rlang::is_scalar_atomic(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    what = c("a non-empty string", "a whole number", "`TRUE`", "`FALSE`"),
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
