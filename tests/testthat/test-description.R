test_that("find_author() works", {
  withr::local_options(list(mark.author = NULL))
  expect_error(find_author())
})

test_that("use_author() works", {
  author_info <- list(
    given = "Jordan Mark",
    family = "Barbone",
    role = c("aut","cph", "cre"),
    email = "jmbarbone@gmail.com",
    comment = c(ORCID = "0000-0001-9788-3628")
  )
  withr::local_options(list(
    mark.check_interactive = NA,
    mark.author = author_info
  ))
  withr::local_dir(test_path("files"))

  expect_identical(find_author(), author_info)

  # get original version
  original <- readLines("DESCRIPTION")
  use_author(author_info)
  # restore

  expect_identical(get_version(), as.package_version("0.0.0"))
  bump_version()
  expect_identical(get_version(), as.package_version("0.0.1"))

  update_version("1.0.0")
  expect_identical(get_version(), as.package_version("1.0.0"))

  update_version("0.2021.10.10")
  expect_identical(get_version(), as.package_version("0.2021.10.10"))

  # first time updates to current date
  bump_date_version()
  string <- paste0("0.", format(Sys.Date(), "%Y.%m.%d"))
  expect_identical(get_version(), as.package_version(string))

  # next run appends another number
  bump_date_version()
  string <- paste0("0.", format(Sys.Date(), "%Y.%m.%d"), ".1")
  expect_identical(get_version(), as.package_version(string))

  writeLines(original, "DESCRIPTION")

  expect_error(use_author(NULL), "list")
  expect_error(use_author(person()), "person")
})
