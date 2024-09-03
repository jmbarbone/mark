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
  expect_error(read_bib(temp), class = "readBibEntriesError")

  expect_error(as_bib(1:3), "data.frame", class = "asBibClassError")
  expect_error(as_bib_list(1:3), "list",  class = "asBibListClassError")

  expect_error(
    process_bib_dataframe(
      categories = list(c("a", "a", "b")),
      values = 1,
      fields = "this",
      keys = "key"
    ),
    class = "processBibDataframeDupeError"
  )
})

test_that("snapshots()", {
  bib_df <- read_bib(test_path("example_bib.txt"))
  bib_list <- attr(bib_df, "bib_list")
  bib_entry <- bib_list[[1]]

  expect_snapshot(print(bib_df))
  expect_snapshot(print(bib_list))
  expect_snapshot(print(bib_entry))
})

test_that("= inside text [#117]", {
  res <- read_bib(textConnection("
    @article{key,
    author     = {Barbone, Jordan Mark},
    title      = {I wrote a cool article},
    year       = {2020},
    month      = {Mar},
    note       = {This has an = and it's bad},
  }"))

  exp <- structure(
    list(
      key = "key",
      field = "article",
      author = "Barbone, Jordan Mark",
      title = "I wrote a cool article",
      year = "2020",
      month = "Mar",
      note = "This has an = and it's bad"
    ),
    class = c("mark_bib_df", "data.frame"),
    row.names = c(NA, -1L)
  )

  expect_identical(res, exp, ignore_attr = "bib_list")
})
