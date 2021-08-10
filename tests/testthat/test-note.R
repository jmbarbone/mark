co_note <- function(x) {
  out <- capture.output(print_note(x))

  if (use_color()) {
    out <- crayon::strip_style(out)
  }

  out
}

inv_co <- function(x) {
  invisible(capture.output(x))
}

test_that("note() work", {
  x <- "x"
  nt <- "this is a note"
  note(x) <- nt
  out <- note(x)

  expect_visible(out)
  expect_s3_class(x, "noted")
  expect_s3_class(out, "note")
  expect_equal(as.character(out), nt)
  inv_co(expect_null(print_note(x)))

  # NULL removes note and class noted
  note(x) <- NULL
  expect_null(attr(x, "note"))
  expect_identical(class(x), "character")
})

test_that("print_note() works with data.frame", {
  x <- data.frame(a = 1:2, b = 1:2)
  note(x) <- "This should work"
  inv_co(expect_null(print_note(x)))

  x <- list(a = 1:3, b = 2, c = data.frame(a = 1))
  note(x) <- "This is a list"
  inv_co(expect_null(print_note(x)))
})

test_that("note() snapshots", {
  x <- 1L
  note(x) <- "snapshot vector"
  expect_snapshot(co_note(x))

  x <- quick_dfl(a = 1:2, b = 1:2)
  note(x) <- "snapshot data.frame"
  expect_snapshot(co_note(x))

  x <- list(a = 1:2, b = 1:2, c = quick_dfl(a = 1, b = 2))
  note(x) <- "snapshot list"
  expect_snapshot(co_note(x))
})
