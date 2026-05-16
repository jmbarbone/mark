test_that("match extensions works", {
  res <- 1:10 %in% c(1, 3, 5, 9)
  exp <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
  expect_identical(res, exp)

  res <- 1:10 %out% c(1, 3, 5, 9)
  expect_identical(res, !exp)

  res <- letters[1:5] %wo% letters[3:7]
  exp <- c("a", "b")
  expect_identical(res, exp)

  res <- letters[1:5] %wi% letters[3:7]
  exp <- c("c", "d", "e")
  expect_identical(res, exp)

  expect_true(any_match(2:3, 1:4))
  expect_false(any_match(2:3, 4))
})
