test_that("paste_combine() works", {
  x <- letters[1:2]
  y <- 1:2
  z <- month.abb[1:2]

  expect_equal(
    paste_combine(x, y),
    c("a1", "a2", "b1", "b2")
  )

  expect_equal(
    paste_combine(x, y, z),
    c("a1Jan", "a1Feb", "a2Jan", "a2Feb",
      "b1Jan", "b1Feb", "b2Jan", "b2Feb")
  )

  expect_equal(
    paste_combine(x, y, z, sep = "."),
    c("a.1.Jan", "a.1.Feb", "a.2.Jan", "a.2.Feb",
      "b.1.Jan", "b.1.Feb", "b.2.Jan", "b.2.Feb")
  )

  expect_equal(
    paste_combine(x, y, sep = "_"),
    c("a_1", "a_2", "b_1", "b_2")
  )

  expect_equal(
    paste_combine(x, y, collate = FALSE),
    c("a1", "b1", "a2", "b2")
  )

  expect_equal(
    paste_combine(x, y, z, collate = FALSE),
    c("a1Jan", "b1Jan", "a2Jan", "b2Jan",
      "a1Feb", "b1Feb", "a2Feb", "b2Feb")
  )
})

test_that("paste_combine() fails", {
  expect_error(paste_combine(1), class = "pasteCombineLengthError")
})

test_that("collapse0()", {
  expect_equal(
    collapse0(list(1:3, letters[1:3]), 5:7, letters[5:7]),
    "123abc567efg"
  )

  expect_equal(
    collapse0(1:3, letters[5:7], sep = "_"),
    "1_2_3_e_f_g"
  )
})

test_that("deprecated", {
  expect_warning(paste_c(1, 2), "deprecated")
})
