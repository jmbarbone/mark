test_that("multi_grepl() examples are correct", {
  x <- c("apple", "banana", "lemon")

  res1 <- multi_grepl(x, c("a" = "^[ab]", "b" = "lem"))
  res2 <- multi_grepl(x, c("a" = "^[ab]", "b" = "q")) # lemon not matches on either
  res3 <- multi_grepl(x, c("a" = "^[ab]", "b" = "e")) # apple matches "a" before "b"
  res3_1 <- multi_grepl(x, c("a" = "^[ab]", "b" = "e"), simplify = FALSE)
  res3_2 <- multi_grepl(x, c("^[ab]", "e"), simplify = FALSE)
  res4 <- multi_grepl(x, c("^[ab]", "e")) # returned as positions

  expect_equal(res1, c("a", "a", "b"))
  expect_equal(res2, c("a", "a", NA_character_))
  expect_equal(res3, c("a", "a", "b"))
  expect_equal(res3_1, list(c("a", "b"), "a", "b"))
  expect_equal(res3_2, list(c(1L, 2L), 1L, 2L))
  expect_equal(res4, c(1L, 1L, 2L))
})
