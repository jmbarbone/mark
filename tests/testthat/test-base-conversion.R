test_that("base_alpha() works as expected", {
  expect_equal(sum(base_alpha(c("x", "k", "c", "d"))), 42)

  expect_equal(base_alpha(letters),
               base_alpha(LETTERS))

  letter_vec <- paste_combine(c("", letters), letters, collate = TRUE)
  expect_equal(base_alpha(letter_vec), 1:702)

  expect_error(base_alpha(letters[1:10], 9))
  expect_warning(alpha_base(letters))
})

test_that("base_n() works as expected", {
  expect_equal(base_n(120, 9), 99)
  expect_error(base_n(62, 2))
})
