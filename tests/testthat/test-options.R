test_that("checkOptions() works", {
  withr::local_options(list(mark.test.option1 = 1L, mark.test.option2 = 2L))

  expect_message(
    checkOptions(list(mark.test.option1 = 2L, mark.test.option2 = 3L))
  )

  expect_identical(getOption("mark.test.option1"), 1L)
  expect_identical(getOption("mark.test.option2"), 2L)

  expect_error(checkOptions(1), info = "is.list(x)")
  expect_error(checkOptions(list(1)), class = "checkOptionsNamesError")
})
