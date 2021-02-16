test_that("Assignments work", {
  x <- "x"
  nt <- "this is a note"
  note(x) <- nt
  out <- note(x)

  expect_visible(out)
  expect_s3_class(x, "noted")
  expect_s3_class(out, "note")
  expect_equal(as.character(out), nt)
})
