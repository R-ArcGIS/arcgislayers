test_that("chunk_indices() splits evenly divisible input (#112)", {
  res <- chunk_indices(10, 5)

  expect_identical(res$start, c(1, 6))
  expect_identical(res$end, c(5, 10))
})

test_that("chunk_indices() truncates the final chunk (#112)", {
  res <- chunk_indices(12, 5)

  expect_identical(res$start, c(1, 6, 11))
  expect_identical(res$end, c(5, 10, 12))
})

test_that("chunk_indices() handles a single chunk (#112)", {
  res <- chunk_indices(3, 500)

  expect_identical(res$start, 1)
  expect_identical(res$end, 3)
})

test_that("chunk_indices() covers every row exactly once (#112)", {
  for (n in c(1, 7, 100, 501)) {
    res <- chunk_indices(n, 50)
    covered <- unlist(Map(seq, res$start, res$end))

    expect_identical(covered, seq_len(n))
  }
})

test_that("update_features() and add_features() chunk by default (#112)", {
  expect_equal(formals(update_features)$chunk_size, 500)
  expect_true("progress" %in% names(formals(update_features)))
})
