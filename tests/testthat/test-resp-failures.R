failed_resp <- function(msg = "Connection timed out") {
  structure(
    class = c("httr2_failure", "httr2_error", "error", "condition"),
    list(message = msg, call = NULL)
  )
}

test_that("check_resp_failures() passes clean responses through", {
  resps <- list(structure(list(), class = "httr2_response"))
  expect_no_error(check_resp_failures(resps))
  expect_identical(check_resp_failures(resps), resps)
})

test_that("check_resp_failures() aborts naming how many pages failed", {
  resps <- list(
    structure(list(), class = "httr2_response"),
    failed_resp(),
    failed_resp()
  )

  expect_error(check_resp_failures(resps), "2 of 3")
})

test_that("check_resp_failures() surfaces the underlying message", {
  resps <- list(failed_resp("Connection timed out"))
  expect_error(check_resp_failures(resps), "Connection timed out")
})

test_that("check_resp_failures() tolerates an empty list", {
  expect_no_error(check_resp_failures(list()))
})
