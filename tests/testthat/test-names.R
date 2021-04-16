test_that("names_switch() works", {
  x <- c(a = 1, b = 2, c = 3)
  y <- names(x)
  names(y) <- x

  expect_equal(names_switch(x), y)

  expect_equal(
    names_switch(list(a = 1:2, b = 2)),
    c(`1:2` = "a", `2` = "b")
  )
})

