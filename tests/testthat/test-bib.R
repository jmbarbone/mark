test_that("read_bib()", {
  # need to include
  res <- read_bib(test_path("example_bib.txt"))
  exp <- read.csv(
    test_path("bib_result.csv"),
    na.strings = "",
    colClasses = "character"
  )
  expect_identical(res, exp, ignore_attr = TRUE)

  res <- read_bib(test_path("example_bib.txt"), max_lines = 148)
  exp <- utils::head(exp, -1L)
  expect_identical(res, exp, ignore_attr = TRUE)

  temp <- tempfile()
  writeLines("bad", temp)
  expect_error(read_bib(temp), "No entries")


  expect_error(as_bib(1:3), "data.frame")
  expect_error(as_bib_list(1:3), "list")

  # TODO add bib with duplicate field

})

test_that("snapshots()", {
  bib <- read_bib(test_path("example_bib.txt"))

  expect_snapshot(print(bib))
  expect_snapshot(print(bib, list = TRUE))
})
