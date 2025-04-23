test_that("file_copy_md5() works", {
  x <- c(tempfile("one"), tempfile("two"), tempfile("three"))
  y <- c(tempfile("one"), tempfile("two"), tempfile("three"))
  on.exit(fs::file_delete(c(x, y)))

  writeLines("one", x[1L])
  writeLines("two", x[2L])
  writeLines("three", x[3L])

  # none of y exists
  expect_message(
    file_copy_md5(x, y, overwrite = FALSE),
    class = fuj_message()
  )
  expect_message(
    file_copy_md5(x, y, overwrite = FALSE),
    class = fuj_message()
  )

  # all of y exists
  expect_message(
    file_copy_md5(x, y, overwrite = TRUE),
    class = fuj_message()
  )

  # mix
  writeLines("twotwo", y[2L])
  fs::file_delete(y[3L])

  expect_message(
    expect_identical(
      attr(file_copy_md5(x, y), "changed"),
      c(FALSE, TRUE, NA)
    ),
    class = fuj_message(),
    regexp = paste(
      "one", "md5 same", "two", "md5 change", "three", "new file",
      sep = ".*"
    )
  )

  expect_message(file_copy_md5(x, y, quiet = TRUE), NA)
})
