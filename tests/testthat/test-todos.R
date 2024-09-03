test_that("todos() works", {
  withr::local_options(list(mark.todos.force = TRUE))

  path <- test_path("scripts")
  file <- fs::path(test_path("scripts/todos.R"))

  res <- todos(path = path)
  exp <- struct(
    list(
      line = c(3L, 7L),
      file = fs::path(rep(file, 2)),
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

test_that("todos() ignores files", {
  path <- test_path("scripts")
  expect_message(
    todos(path = path, ignore = "todo", force = TRUE),
    "No todos found"
  )
})

test_that("todos() errors and messages", {
  withr::local_options(list(mark.todos.force = TRUE))
  path <- test_path("scripts")

  err <- "path must be a character vector of length 1L"
  expect_error(todos(path = 1), class = "doTodoPathError")
  expect_error(todos(path = c("a", "b")), class = "doTodoPathError")
  expect_error(todos(path = "zzz"))
  expect_error(do_todo(c("todo", "fixme"), path = "."), NA)

  expect_message(res <- todos("zzzzzz", path = path), "No todos found")
  expect_null(res)

  withr::local_options(list(mark.todos..norm_path = FALSE))
  expect_snapshot(todos(path = path))
})
