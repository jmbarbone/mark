context("String utility testing")

test_that("str-close-enough example is correct.", {
  res1 <- str_close_enough(c("thsi", "TIHS", "that"), "this", negate = F)
  res2 <- str_close_enough(c("thsi", "TIHS", "that"), "this", negate = T)

  expect_equal(res1, c(TRUE, TRUE, FALSE))
  expect_equal(res2, c(FALSE, FALSE, TRUE))
})
