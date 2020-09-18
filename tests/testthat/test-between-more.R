context("Between more")

test_that("Examples are accurate", {
  expect_false(between_more(10, 2, 10, "gl"))
  expect_true(between_more(10, 2, 10, "gle"))
})

test_that("Switches work", {
  x <- 1:20
  gele <- x >= 5 & x <= 10
  gel  <- x >= 5 & x <  10
  gle  <- x >  5 & x <= 10
  gl   <- x >  5 & x <  10

  expect_equal(between_more(x, 5, 10, "gele"), gele)
  expect_equal(between_more(x, 5, 10, "gel"),  gel)
  expect_equal(between_more(x, 5, 10, "gle"),  gle)
  expect_equal(between_more(x, 5, 10, "gl"),   gl)
})
