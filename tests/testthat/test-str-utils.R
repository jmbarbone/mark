context("String utility testing")

test_that("str-close-enough example is correct.", {
  res1 <- str_close_enough(c("thsi", "TIHS", "that"), "this", negate = F)
  res2 <- str_close_enough(c("thsi", "TIHS", "that"), "this", negate = T)

  expect_equal(res1, c(TRUE, TRUE, FALSE))
  expect_equal(res2, c(FALSE, FALSE, TRUE))
})

test_that("string slices", {
  x <- stringi::stri_rand_lipsum(1)
  len <- 77L

  # By length
  res <- str_slice(x, n = len)
  expect_equal(paste(res, collapse = ""), x, info = "collapsed res")
  lengths <- vapply(res, nchar, integer(1))
  expect_true(all(lengths <= len), info = "All lines within desired length")

  # By length and word
  res <- str_slice_by_word(x, n = len)
  expect_equal(paste(res, collapse = ""), x, info = "By word:  Collapsed res")
  lengths <- vapply(res, nchar, integer(1))
  expect_true(all(lengths <= len), info = "By word:  All lines within desired length")
})
