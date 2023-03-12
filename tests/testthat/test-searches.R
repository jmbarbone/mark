test_that("multi_grepl() examples are correct", {
  x <- c("apple", "banana", "lemon")

  expect_equal(
    multi_grepl(x, c("a" = "^[ab]", "b" = "lem")),
    c("a", "a", "b")
  )

  expect_equal(
    # lemon not matches on either
    multi_grepl(x, c("a" = "^[ab]", "b" = "q")),
    c("a", "a", NA_character_)
  )

  expect_equal(
    # apple matches "a" before "b"
    multi_grepl(x, c("a" = "^[ab]", "b" = "e")),
    c("a", "a", "b")
  )

  expect_equal(
    multi_grepl(x, c("a" = "^[ab]", "b" = "e"), simplify = FALSE),
    list(c("a", "b"), "a", "b")
  )

  expect_equal(
    multi_grepl(x, c("^[ab]", "e"), simplify = FALSE),
    list(c(1L, 2L), 1L, 2L)
  )

  expect_equal(
    # returned as positions
    multi_grepl(x, c("^[ab]", "e")),
    c(1L, 1L, 2L)
  )
})
