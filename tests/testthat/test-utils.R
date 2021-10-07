test_that("match param", {
  foo <- function(x = c('a', 'b')) {
    match_param(x)
  }

  expect_equal(foo("a"), "a")
  expect_equal(foo("b"), "b")
  expect_equal(foo(c("a", "b")), "a")
  expect_equal(foo(c("b", "a")), "b")
  expect_error(foo("c"))
})

test_that("utils", {
  expect_true(has_length(1))
  expect_false(has_length(NULL))
  expect_false(has_length(integer()))

  expect_true(is_unique(1:2))
  expect_false(is_unique(c(1, 1)))

  res <- remove_class(struct(1, class = "foo"))
  expect_identical(class(res), "numeric")

  expect_identical(append0(1:3, 4L), 1:4)
})