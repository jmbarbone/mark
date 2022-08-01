co_note <- function(x) {
  op <- options(mark.check_interactive = FALSE)
  on.exit(options(op), add = TRUE)
  out <- capture.output(print_note(x))

  if (use_color()) {
    out <- crayon::strip_style(out)
  }

  out
}

test_that("note() work", {
  withr::local_options(list(mark.check_interactive = NA))

  x <- "x"
  nt <- "this is a note"
  note(x) <- nt
  out <- note(x)

  expect_visible(out)
  expect_s3_class(x, "noted")
  expect_s3_class(out, "note")
  expect_equal(as.character(out), nt)
  expect_identical(print_note(x), x)

  # NULL removes note and class noted
  note(x) <- NULL
  expect_null(attr(x, "note"))
  expect_identical(class(x), "character")

  x <- "x"
  y <- set_note(x, "this note")
  note(x) <- "this note"
  expect_identical(x, y)
})

test_that("print.noted() passes to next methods [67] (data.frame)", {
  x <- data.frame(a = 1:50)
  original <- capture.output(print(x, max = 5))
  note(x) <- "note"
  co <- withr::with_options(list(mark.check_interactive = FALSE), {
    capture.output(print(x, max = 5))
  })
  expect_true(all(original %in% co))
  expect_identical(co[1], "Note : note")
})

test_that("print.noted() passes to next methods [67] (tibble)", {
  skip_on_cran() # don't need {tibble} of {pillar} breaking this test
  skip_if_not_installed("tibble")

  # not bothering with snapshots
  x <- tibble::tibble(a = 1:50)
  original <- capture.output(print(x, n = 40))
  note(x) <- "note"
  co <- withr::with_options(list(mark.check_interactive = FALSE), {
    capture.output(print(x, n = 40))
  })
  expect_true(all(original[-3] %in% co))
  expect_identical(co[1], "Note : note")
})

test_that("print_note() works with data.frame", {
  withr::local_options(list(mark.check_interactive = NA))

  x <- data.frame(a = 1:2, b = 1:2)
  note(x) <- "This should work"
  expect_identical(print_note(x), x)

  x <- list(a = 1:3, b = 2, c = data.frame(a = 1))
  note(x) <- "This is a list"
  expect_identical(print_note(x), x)
})

test_that("note() errors", {
  withr::local_options(list(mark.check_interactive = NA))
  expect_error(print_note(1L))
  x <- struct(1L, "noted", note = struct("hi", "note_a_note"))
  expect_error(print_note(x))
})

test_that("note() snapshots", {
  withr::local_options(list(mark.check_interactive = FALSE))

  x <- 1L
  note(x) <- "snapshot vector"
  expect_snapshot(x)

  x <- quick_dfl(a = 1:2, b = 1:2)
  note(x) <- "snapshot data.frame"
  expect_snapshot(x)

  x <- list(a = 1:2, b = 1:2, c = quick_dfl(a = 1, b = 2))
  note(x) <- "snapshot list"
  expect_snapshot(x)

  expect_snapshot(struct(1L, "note"))
})
