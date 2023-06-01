rawstr <- readLines("tests/testthat/data/new-field-types.json") |>
  paste(collapse = "")

res <- parse_esri_json(rawstr, int64_policy = "integer64")

as.Date(res$dateOnly)
# have to ignore offset as far as i can tell
as.POSIXct(res$tstDToffset, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

hms::as_hms(res$timeOnly)

restore_integer64 <- function(x) structure(x, class = "integer64")

library(clock)

# testing date time offset

