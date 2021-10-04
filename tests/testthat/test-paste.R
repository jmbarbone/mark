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
    c("a11Jan", "a11Feb", "a12Jan", "a12Feb",
      "a21Jan", "a21Feb", "a22Jan", "a22Feb",
      "b11Jan", "b11Feb", "b12Jan", "b12Feb",
      "b21Jan", "b21Feb", "b22Jan", "b22Feb")
  )

  expect_equal(
    paste_combine(x, y, z, sep = "."),
    c("a.1.1.Jan", "a.1.1.Feb", "a.1.2.Jan", "a.1.2.Feb",
      "a.2.1.Jan", "a.2.1.Feb", "a.2.2.Jan", "a.2.2.Feb",
      "b.1.1.Jan", "b.1.1.Feb", "b.1.2.Jan", "b.1.2.Feb",
      "b.2.1.Jan", "b.2.1.Feb", "b.2.2.Jan", "b.2.2.Feb")
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
    c("a11Jan", "b11Jan", "a21Jan", "b21Jan",
      "a12Jan", "b12Jan", "a22Jan", "b22Jan",
      "a11Feb", "b11Feb", "a21Feb", "b21Feb",
      "a12Feb", "b12Feb", "a22Feb", "b22Feb")
  )
})

test_that("paste_combine() fails", {
  expect_error(paste_combine(1))
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
