test_that("base_alpha() works as expected", {
  expect_equal(sum(base_alpha(c("x", "k", "c", "d"))), 42)

  expect_equal(base_alpha(letters), base_alpha(LETTERS))

  letter_vec <- paste_combine(c("", letters), letters, collate = TRUE)
  expect_equal(base_alpha(letter_vec), 1:702)

  expect_error(base_alpha(letters[1:10], 9), class = "baseAlphaLimitError")
  expect_warning(alpha_base(letters), "deprecated")

  expect_identical(base_alpha_single("a", 26), 1L)
  expect_identical(base_alpha_single("j", 26), 10L)
  expect_identical(base_alpha_single("aa", 26), 27L)
  expect_identical(base_alpha_single("1", 1), NA_integer_)
})

test_that("base_n() works as expected", {
  expect_equal(base_n(120, 9), 99)
  expect_error(base_n(62, 2), class = "baseNSingleLimitError")

  x <- c(2L, 1L, 3L, 3L, 6L, 5L, 1L, 4L, 4L, 2L, 2L, 6L, 7L, 4L, 4L, 3L, 1L, 1L)
  expect_identical(base_n(x, 5, 5), x)
})

test_that("base_alpha(), base_n() fails", {
  expect_error(base_alpha(1), class = "simpleError")
  expect_error(base_n("a"), class = "simpleError")
  expect_error(base_n(1, 10, 12), class = "baseNTenError")
})

test_that("check_base(), check_base_alpha() works", {
  expect_error(check_base(1.1), class = "checkBaseIntegerError")
  expect_error(check_base(-1, 9), class = "checkBaseLimitError")
  expect_error(check_base(10, 9), class = "checkBaseLimitError")

  expect_null(check_base_alpha("j"))
  expect_error(check_base_alpha("ab"), class = "checkBaseAlphaLengthError")
})
