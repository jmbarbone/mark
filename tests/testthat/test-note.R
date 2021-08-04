test_that("note() work", {
  x <- "x"
  nt <- "this is a note"
  note(x) <- nt
  out <- note(x)

  expect_visible(out)
  expect_s3_class(x, "noted")
  expect_s3_class(out, "note")
  expect_equal(as.character(out), nt)

  # NULL removes note and class noted
  note(x) <- NULL
  expect_null(attr(x, "note"))
  expect_identical(class(x), "character")
})

test_that("note() snapshots", {
  x <- 1L
  note(x) <- "snapshot vector"
  expect_snapshot_output(x, cran = TRUE)
  expect_snapshot_output(note(x), cran = TRUE)

  x <- quick_dfl(a = 1.0)
  note(x) <- "snapshot data.frame"
  expect_snapshot_output(x, cran = TRUE)
})
