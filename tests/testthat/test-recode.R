test_that("recode() examples works", {
  expect_equal(
    recode_by(1:3, c(a = 1, b = 2)),
    c("a", "b", NA_character_)
  )
  expect_equal(
    recode_by(letters[1:3], c(`1` = "a", `2` = "b")),
    c("1", "2", NA_character_)
  )

  x <- recode_by(letters[1:3], c(`1` = "a", `2` = "b"), mode = "integer")
  expect_equal(x, c(1, 2, NA_integer_))
  expect_equal(x, recode_by(letters[1:3], c("a", "b"), vals = 1:2))
})
