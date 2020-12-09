test_that("Assignments work", {
  op <- options()

  # To default
  options(jordan.note.fun = NULL)

  x <- "x"
  nt <- "this is a note"
  note(x) <- nt
  out <- suppressMessages(note(x))

  expect_visible(out)
  expect_s3_class(out, "jordan_note")
  expect_equal(as.character(out), nt)

  on.exit(do.call(options, as.list(op)))
})

test_that("Options", {
  op <- options()
  # setting to default
  options(jordan.note.fun = NULL)

  x <- "x"
  note(x) <- "note"
  expect_invisible(suppressMessages(note(x)))
  expect_message(note(x))

  options(jordan.note.fun = message)
  expect_message(note(x))

  options(jordan.note.fun = warning)
  expect_warning(note(x))

  options(jordan.note.fun = stop)
  expect_error(note(x))

  options(jordan.note.fun = "not a function")
  expect_error(note(x))

  on.exit(do.call(options, as.list(op)))
})
