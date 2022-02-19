test_that("recode_by() examples works", {
  expect_equal(
    recode_by(1:3, c(a = 1, b = 2)),
    c("a", "b", NA_character_)
  )
  expect_equal(
    recode_by(letters[1:3], c(`1` = "a", `2` = "b")),
    c("1", "2", NA_character_)
  )

  x <- recode_by(letters[1:3], c(`1` = "a", `2` = "b"), mode = "integer")
  expect_equal(x, c(1, 2, NA_integer_))
  expect_equal(x, recode_by(letters[1:3], c("a", "b"), vals = 1:2))
})

test_that("recode_only() examples work", {
  expect_equal(
    recode_only(letters[1:3], c("zzz" = "a")),
    c("zzz", "b", "c")
  )

  expect_equal(
    recode_only(letters[1:3], c(`1` = "a")),
    c("1", "b", "c")
  )

  expect_equal(
    wuffle(recode_only(1:3, c("a" = 1))),
    c(NA_integer_, 2, 3)
  )
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
