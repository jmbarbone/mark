test_that("read_bib()", {
  # need to include
  bib <- read_bib(test_path("example_bib.txt"))
  exp <- read.csv(test_path("bib_result.csv"),
                  na.strings = "",
                  colClasses = "character")
  expect_equal(bib, exp, ignore_attr = TRUE)
})
