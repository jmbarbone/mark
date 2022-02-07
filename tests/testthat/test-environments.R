test_that("environments() and friends works", {
  expect_error(environments(), NA)

  x <- ls_all()
  expect_true(all(vap_lgl(unlist(x), exists)))

  ne <- new.env()

  local({
    foo_obj <- structure(list(), class = "foo")
    foo_fun <- function() NULL
    foo_df <- data.frame(a = 1)
  }, envir = ne)

  # these are failing...?
  expect_identical(ls_function(envir = ne), "foo_fun")
  expect_identical(ls_object(envir = ne), c("foo_df", "foo_obj"))
  expect_identical(ls_dataframe(envir = ne), "foo_df")

  expect_identical(
    make_do_ls("is.object"),
    ls_object,
    ignore_function_env = TRUE
  )
})

test_that("snapshots", {
  expect_error(print(environments()), NA)
  skip("not static")
  expect_snapshot(environments())
})
