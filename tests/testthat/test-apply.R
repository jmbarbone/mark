times <- function(x, n = 2L) {
  x * n
}

test_that("vap_int()", {
  expect_equal(
    vapply(1:10, times, integer(1)),
    vap_int(1:10, times)
  )

  # Adds names
  expect_equal(
    names(vap_int(1:10, times, .nm = TRUE)),
    as.character(1:10)
  )

  x <- 1:10
  names(x) <- letters[1:10]
  expect_equal(
    names(vap_int(x, times, .nm = TRUE)),
    letters[1:10]
  )
})


test_that("vap_dbl()", {
  expect_equal(
    vapply(1:10, times, n = 2, double(1)),
    vap_dbl(1:10, times)
  )

  # Adds names
  expect_equal(
    names(vap_dbl(1:10, times, n = 2, .nm = TRUE)),
    as.character(1:10)
  )

  x <- 1:10
  names(x) <- letters[1:10]
  expect_equal(
    names(vap_dbl(x, times, n = 2, .nm = TRUE)),
    letters[1:10]
  )
})

test_that("vap_cplx()", {
  foo <- function(x) {
    x + 1i
  }

  expect_equal(
    vapply(1:10, foo, complex(1)),
    vap_cplx(1:10, foo)
  )

  # Adds names
  expect_equal(
    names(vap_cplx(1:10, foo, .nm = TRUE)),
    as.character(1:10)
  )

  x <- 1:10
  names(x) <- letters[1:10]
  expect_equal(
    names(vap_cplx(x, foo, .nm = TRUE)),
    letters[1:10]
  )
})

test_that("vap_chr()", {
  foo <- function(x) {
    letters[x]
  }

  expect_equal(
    vapply(1:10, foo, character(1)),
    vap_chr(1:10, foo)
  )

  # Adds names
  expect_equal(
    names(vap_chr(1:10, foo, .nm = TRUE)),
    as.character(1:10)
  )

  x <- 1:10
  names(x) <- letters[1:10]
  expect_equal(
    names(vap_chr(x, foo, .nm = TRUE)),
    letters[1:10]
  )
})

test_that("vap_date", {
  foo <- function(x) {
    Sys.Date() + x
  }

  expect_s3_class(
    vap_date(1:10, foo),
    "Date"
  )

  # Adds names
  expect_equal(
    names(vap_date(1:10, foo, .nm = TRUE)),
    as.character(1:10)
  )

  x <- 1:10
  names(x) <- letters[1:10]
  expect_equal(
    names(vap_date(x, foo, .nm = TRUE)),
    letters[1:10]
  )
})

test_that("vap_lgl()", {
  foo <- function(x) {
    c(FALSE, TRUE)[x %% 2 + 1]
  }

  expect_equal(
    vapply(1:10, foo, logical(1)),
    vap_lgl(1:10, foo)
  )

  # Adds names
  expect_equal(
    names(vap_lgl(1:10, foo, .nm = TRUE)),
    as.character(1:10)
  )

  x <- 1:10
  names(x) <- letters[1:10]
  expect_equal(
    names(vap_lgl(x, foo, .nm = TRUE)),
    letters[1:10]
  )
})
