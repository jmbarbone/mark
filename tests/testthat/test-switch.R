test_that("switch_params() works as expected", {
  res <- switch_params(c("j", "m", "b"), j = 10, b = 2, m = 13)
  expect_equal(res, c(j = 10, m = 13, b = 2))
})

test_that("switch_case() works as expected", {
  res <- switch_case(
    1:5 == 4      ~ NA_integer_, # 4
    1:5 %% 2 == 0 ~ 1:5,         # 2 & 4
    1:5 == 1      ~ 6:10,        # 1
    .default = -1L               # 3 & 5
  )

  expect_equal(res, c(6L, 2L, -1L, NA_integer_, -1L))

  res <- switch_case(
    1:10 < 3 ~ 1,
    1:10 < 5 ~ 2,
    .default = NA_integer_
  )

  expect_equal(res, c(rep(1, 2), rep(2, 2), rep(NA_integer_, 6)))
})

test_that("switch_in_case() works as expected", {
  x <- c(1, 2, 12, 4, 20, 21)
  res <- switch_in_case(x, 1:10 ~ 1, 11:20 ~ 2)
  exp <- set_names0(c(1, 1, 2, 1, 2, NA), x)
  expect_equal(res, exp)

  x <- c("a", "b", "d", "e", "g", "j")
  res <- switch_in_case(x, letters[1:3] ~ "a", letters[5:6] ~ "e")
  exp <- set_names0(c("a", "a", NA, "e", NA, NA), x)
  expect_equal(res, exp)

  # handles functions
  res <- switch_in_case(1:6, c(1, 3, 5) ~ exp, c(2, 4) ~ log)
  exp <- c(exp(1), log(2), exp(3), log(4), exp(5), NA)
  names(exp) <- 1:6
  expect_equal(res, exp)
})

test_that("switch_in_case() handles evaluations", {
  use_these <- c(1, 3, 2, 5)
  x <- 1:6
  res <- switch_in_case(x, use_these ~ TRUE, .default = FALSE)
  expect_equal(res, set_names0(x %in% use_these, x))

  x <- seq.int(1, 60, 6)
  res <- switch_in_case(
    x,
    1:10 ~ "a",
    11:20 ~ "b",
    c(22, 24, 26) ~ "c",
    30:Inf ~ "d"
  )
  exp_res <- set_names0(c("a", "a", "b", "b", NA, "d", "d", "d", "d", "d"), x)
  expect_equal(res, exp_res)

  ne <- new.env()
  ne$use_these2 <- use_these
  expect_error(switch_in_case(1:10, use_these2 ~ TRUE), "use_these2")
  expect_error(switch_in_case(1:10, use_these2 ~ TRUE, .envir = ne), NA)
})
