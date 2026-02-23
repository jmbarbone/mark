# nolint start: spaces_inside_linter.

# fmt: skip
test_that("Logical extension work", {
  x <- c(TRUE, TRUE,  TRUE,  FALSE, FALSE, FALSE, NA,   NA,    NA)
  y <- c(TRUE, FALSE, NA,    TRUE,  FALSE, NA,    TRUE, FALSE, NA)
  z <- c(TRUE, TRUE,  FALSE, FALSE, NA,    NA,    TRUE, FALSE, NA)

  xL <- as.integer(x) # nolint: object_name_linter.
  xD <- as.double(x) # nolint: object_name_linter.

  res_n <- logical(length(x))

  res_xy_and    <- res_n
  res_xy_and_na <- res_n
  res_xy_or     <- res_n
  res_xy_or_na  <- res_n

  res_xyz_and    <- res_n
  res_xyz_and_na <- res_n
  res_xyz_or     <- res_n
  res_xyz_or_na  <- res_n

  for (i in seq_along(x)) {
    res_xy_and[i] <- x[i] & y[i]
    res_xy_or[i]  <- x[i] | y[i]

    res_xyz_and[i] <- x[i] & y[i] & z[i]
    res_xyz_or[i]  <- x[i] | y[i] | z[i]

    res_xy_and_na[i] <- all(x[i], y[i], na.rm = TRUE)
    res_xy_or_na[i]  <- any(x[i], y[i], na.rm = TRUE)

    res_xyz_and_na[i] <- all(x[i], y[i], z[i], na.rm = TRUE)
    res_xyz_or_na[i]  <- any(x[i], y[i], z[i], na.rm = TRUE)
  }

  expect_equal(is_true(x),  sapply(x, isTRUE,  USE.NAMES = FALSE))
  expect_equal(is_false(x), sapply(x, isFALSE, USE.NAMES = FALSE))

  expect_identical(is_true( c(TRUE, FALSE, NA)), c(TRUE,  FALSE, FALSE))
  expect_identical(is_false(c(TRUE, FALSE, NA)), c(FALSE, TRUE,  FALSE))

  expect_true(is_boolean(x))
  expect_true(is_boolean(xL))
  expect_true(is_boolean(xD))

  expect_equal(AND(x, y),               res_xy_and)
  expect_equal(AND(x, y, na.rm = TRUE), res_xy_and_na)

  expect_equal(AND(x, y, z),               res_xyz_and)
  expect_equal(AND(x, y, z, na.rm = TRUE), res_xyz_and_na)

  expect_equal(OR(x, y),               res_xy_or)
  expect_equal(OR(x, y, na.rm = TRUE), res_xy_or_na)

  expect_equal(OR(x, y,    na.rm = TRUE), res_xy_or_na)
  expect_equal(OR(x, y, z, na.rm = TRUE), res_xyz_or_na)
})

test_that("logical helpers", {
  expect_error(check_null(NULL), class = "input_error")
  expect_error(check_null(integer()), class = "input_error")

  expect_error(
    apply_logical_matrix(1L, mean, TRUE),
    class = "input_error"
  )

  expect_error(
    apply_logical_matrix(matrix("a"), mean, TRUE),
    class = "input_error"
  )

  expect_error(
    apply_logical_matrix(matrix(3L), mean, TRUE),
    class = "input_error"
  )
})

test_that("is_true()/is_false()", {
  expect_identical(
    is_true(c(TRUE, FALSE, NA)),
    c(TRUE, FALSE, FALSE)
  )

  expect_identical(
    is_false(c(TRUE, FALSE, NA)),
    c(FALSE, TRUE, FALSE)
  )

  expect_identical(is_true(1:10), rep(NA, 10))
  expect_identical(is_false(1:10), rep(NA, 10))
})

test_that("logic gates works", {
  x <- rep(c(TRUE, FALSE, NA), 4)
  y <- rep(c(FALSE, TRUE, NA), each = 4)

  expect_identical(
    nor(x, y),
    !((x & y) | xor(x, y) | x | y)
  )

  expect_identical(
    nand(x, y),
    (!x & !y) | (!x & (y | is.na(y))) | ((x | is.na(x)) & !y)
  )

  expect_identical(
    xnandr(x, y),
    (x & y) | (!x & !y)
  )
})

test_that("either() works", {
  x <- c(TRUE, NA, FALSE, FALSE)
  y <- c(TRUE, TRUE, FALSE, NA)

  res <- either(x, y)
  exp <- c(TRUE, TRUE, FALSE, FALSE)
  expect_identical(res, exp)
})

test_that("none() works", {
  x <- c(FALSE, FALSE, NA)
  expect_identical(none(x), NA)
  expect_true(none(x, na.rm = TRUE))

  x <- c(TRUE, NA)
  expect_false(none(x))
  expect_false(none(x, na.rm = TRUE))
})

test_that("logical helpers", {
  expect_error(apply_logical_matrix(1L, mean, TRUE), class = "input_error")
  expect_error(
    apply_logical_matrix(matrix("a"), mean, TRUE),
    class = "input_error"
  )
  expect_error(
    apply_logical_matrix(matrix(3L), mean, TRUE),
    class = "input_error"
  )
})
