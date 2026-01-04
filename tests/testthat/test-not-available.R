test_that("not_available() works", {
  expect_error(get_not_available(), NA)
  expect_error(
    get_not_available("foo"),
    class = "mark:not_available_error"
  )

  val <- struct(NA, "foo")
  set_not_available("foo", val)

  expect_identical(get_not_available("foo"), val)
  expect_identical(not_available("foo", 2), c(NA, NA))
  expect_identical(not_available(val, 2), c(NA, NA))

  expect_true(is.na(not_available("Date", 1)))

  set_not_available("foo", NULL)
  set_not_available("foo_fun", function() NULL)
  expect_error(
    get_not_available("foo_fun"),
    class = "mark:not_available_error"
  )

  # reset list
  options(mark.na_list = NULL)
  expect_identical(get_na_list(), op.mark$mark.na_list)
})
