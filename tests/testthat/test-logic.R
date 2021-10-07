test_that("Logical extension work", {
  x <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA, NA)
  y <- c(TRUE, FALSE, NA, TRUE, FALSE, NA, TRUE, FALSE, NA)
  z <- c(TRUE, TRUE, FALSE, FALSE, NA, NA, TRUE, FALSE, NA)

  xL <- as.integer(x)
  xD <- as.double(x)

  res_n <- logical(length(x))

  res_xy_and     <- res_n
  res_xy_and_na  <- res_n
  res_xy_or      <- res_n
  res_xy_or_na   <- res_n

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
    res_xy_or_na[i] <- any(x[i], y[i], na.rm = TRUE)

    res_xyz_and_na[i] <- all(x[i], y[i], z[i], na.rm = TRUE)
    res_xyz_or_na[i] <- any(x[i], y[i], z[i], na.rm = TRUE)
  }

  expect_equal(is_true(x), sapply(x, isTRUE, USE.NAMES = FALSE))
  expect_equal(is_false(x), sapply(x, isFALSE, USE.NAMES = FALSE))

  expect_identical(is_true(c( TRUE, FALSE, NA)), c(TRUE,  FALSE, FALSE))
  expect_identical(is_false(c(TRUE, FALSE, NA)), c(FALSE,  TRUE, FALSE))

  expect_true(is_boolean(x))
  expect_true(is_boolean(xL))
  expect_true(is_boolean(xD))

  expect_equal(AND(x, y), res_xy_and)
  expect_equal(AND(x, y, na.rm = TRUE), res_xy_and_na)

  expect_equal(AND(x, y, z), res_xyz_and)
  expect_equal(AND(x, y, z, na.rm = TRUE), res_xyz_and_na)

  expect_equal(OR(x, y), res_xy_or)
  expect_equal(OR(x, y, na.rm = TRUE), res_xy_or_na)

  expect_equal(OR(x, y, na.rm = TRUE), res_xy_or_na)
  expect_equal(OR(x, y, z, na.rm = TRUE), res_xyz_or_na)
})

test_that("logical helpers", {
  expect_error(null_check(NULL))
  expect_error(null_check(integer()))

  expect_error(apply_logical_matrix(1L, mean, TRUE), "must be a matrix")
  expect_error(apply_logical_matrix(matrix("a"), mean, TRUE), "must be boolean")
  expect_error(apply_logical_matrix(matrix(3L), mean, TRUE), "must be boolean")
})
