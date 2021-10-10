test_that("lines_of_r_code() works", {
  x <- test_path("scripts/lines")
  expect_equal(lines_of_r_code(x), 6L)
  expect_equal(lines_of_r_code(x, skip_empty = FALSE), 13L)

  x <-  test_path("scripts/lines/lines.R")
  expect_equal(lines_of_r_code(x), 4L)
})
