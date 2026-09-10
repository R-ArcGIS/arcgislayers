attach_df <- function(...) {
  base <- data.frame(
    name = "a.png",
    url = "https://example.com/a.png",
    contentType = "image/png"
  )
  utils::modifyList(base, list(...))
}

test_that("download_attachments() rejects missing columns", {
  for (col in c("name", "url", "contentType")) {
    df <- attach_df()
    df[[col]] <- NULL

    expect_error(download_attachments(df, tempdir()), col)
  }
})

test_that("download_attachments() rejects NA values", {
  for (col in c("name", "url", "contentType")) {
    df <- attach_df()
    df[[col]] <- NA_character_

    expect_error(download_attachments(df, tempdir()), col)
  }
})

test_that("download_attachments() rejects empty strings", {
  for (col in c("name", "url", "contentType")) {
    df <- attach_df()
    df[[col]] <- ""

    expect_error(download_attachments(df, tempdir()), col)
  }
})

test_that("download_attachments() rejects non-character columns", {
  expect_error(download_attachments(attach_df(name = 1L), tempdir()), "character")
})
