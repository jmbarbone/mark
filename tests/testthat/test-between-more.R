test_that("Examples are accurate", {
  expect_false(between_more(10, 2, 10, "gl"))
  expect_true(between_more(10, 2, 10, "gle"))
})

test_that("between_more() works", {
  x <- 1:20
  gele <- x >= 5 & x <= 10
  gel  <- x >= 5 & x <  10
  gle  <- x >  5 & x <= 10
  gl   <- x >  5 & x <  10

  expect_equal(between_more(x, 5, 10, "gele"), gele)
  expect_equal(between_more(x, 5, 10, "gel"),  gel)
  expect_equal(between_more(x, 5, 10, "gle"),  gle)
  expect_equal(between_more(x, 5, 10, "gl"),   gl)

  # vectors for left or right

  x <- 1:5
  left <- c(2, 2, 3, 2, 4)
  right <- c(3, 3, 4, 3, 4)

  res <- c(FALSE, TRUE, TRUE, TRUE, TRUE)
  expect_identical(between_more(x, left, 5), res)

  res <- c(FALSE, FALSE, TRUE, FALSE, FALSE)
  expect_identical(between_more(x, 3, right), res)

  res <- c(FALSE, TRUE, TRUE, FALSE, FALSE)
  expect_identical(between_more(x, left, right), res)

  expect_warning(
    between_more(1:2, 3, 2),
    "`left` > `right`",
    class = "betweenMoreLrWarning"
  )
})
