test_that("recode_by() examples works", {
  res <- recode_by(1:3, c(a = 1, b = 2))
  exp <- c("a", "b", NA_character_)
  expect_identical(res, exp)

  res <- recode_by(letters[1:3], c(`1` = "a", `2` = "b"))
  exp <-  c("1", "2", NA_character_)
  expect_identical(res, exp)

  res <- recode_by(letters[1:3], c(`1` = "a", `2` = "b"), mode = "integer")
  exp <- c(1L, 2L, NA_integer_)
  expect_identical(res, exp)
  exp <- recode_by(letters[1:3], c("a", "b"), vals = 1:2)
  expect_identical(res, exp)
})

test_that("recode_only() examples work", {
  res <- recode_only(letters[1:3], c("zzz" = "a"))
  exp <- c("zzz", "b", "c")
  expect_identical(res, exp)

  res <- recode_only(letters[1:3], c(`1` = "a"))
  exp <- c("1", "b", "c")
  expect_identical(res, exp)

  res <- wuffle(recode_only(1:3, c(a = 1L)))
  exp <- c(NA_integer_, 2L, 3L)
  expect_identical(res, exp)
})

test_that("clean_na_coercion() works", {
  expect_warning(
    clean_na_coercion(as.vector(c(1, "NA", "a"), "double")),
    "^NAs introduced by coercion$"
  )
})

test_that("errors", {
  txt <- "values to recode by were not properly set"
  expect_error(recode_by(1, 1), txt)
  expect_error(recode_only(1, 1), txt)
})

test_that("single value", {
  x <- letters[c(1, 2, 2, 1, 3)]
  res <- recode_only(x, c("b", "a"), "d")
  exp <- c("d", "d", "d", "d", "c")
  expect_identical(res, exp)

  res <- recode_by(x, c("b", "a"), "d")
  exp <- c("d", "d", "d", "d", NA_character_)
  expect_identical(res, exp)
})

test_that("factors", {
  x <- factor(c("a", "b", "c"))
  res <- recode_only(x, c(new = "new"))
  expect_identical(res, x)

  res <- recode_by(x, c(new = "new"))
  exp <- factor(c(NA, NA, NA))
  expect_identical(res, exp)

  res <- recode_only(x, c(A = "a"))
  exp <- factor(c("A", "b", "c"))
  expect_identical(exp, res)

  res <- recode_by(x, c(A = "a"))
  exp <- factor(c("A", NA, NA))
  expect_identical(res, exp)
})

test_that("recode_*(by = list())", {
  x <- c("a", "b", "c", "d", "e")
  by <- list(xy = c("a", "b"), z = "d")

  exp <- c("xy", "xy", "c", "z", "e")
  res <- recode_only(x, by)
  expect_identical(exp, res)

  exp <- c("xy", "xy", NA, "z", NA)
  res <- recode_by(x, by)
  expect_identical(exp, res)
})
