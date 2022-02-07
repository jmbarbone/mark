test_that("todos() works", {
  withr::local_options(list(mark.todos.force = TRUE))

  path <- test_path("scripts")
  file <- test_path("scripts/todos.R")

  res <- todos(path = path)
  exp <- struct(
    list(
      line = c(3L, 7L),
      file = rep(file, 2),
      todo = c("make x longer", "add another example")
    ),
    c("todos_df", "data.frame"),
    todos_type = "todo",
    row.names = 1:2,
    .keep_attr = TRUE
  )

  expect_identical(res, exp)

  res <- todos(pattern = "example", path = path)
  exp <- struct(
    list(
      line = 7L,
      file = file,
      todo = "add another example"
    ),
    c("todos_df", "data.frame"),
    todos_type = "todo",
    row.names = 1L,
    .keep_attr = TRUE
  )

  expect_identical(res, exp)

  res <- fixmes(path = path)
  exp <- struct(
    list(
      line = 9L,
      file = file,
      fixme = "This is a fixme"
    ),
    c("todos_df", "data.frame"),
    todos_type = "fixme",
    row.names = 1L,
    .keep_attr = TRUE
  )

  expect_identical(res, exp)
})

test_that("todo() errors and messages", {
  withr::local_options(list(mark.todos.force = TRUE))

  path <- test_path("scripts")

  err <- "path must be a character vector of length 1L"
  expect_error(todos(path = 1), err)
  expect_error(todos(path = c("a", "b")), err)
  expect_error(todos(path = "zzz"), "path not found: zzz")
  expect_error(do_todo(c("todo", "fixme"), path = "."), "Length of text must be 1")

  expect_message(res <- todos("zzzzzz", path = path), "No todos found")
  expect_null(res)

  expect_snapshot(todos(path = path))
})
