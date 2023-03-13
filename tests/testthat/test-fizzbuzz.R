test_that("fizzbuzz() works", {

  exp <- c(
    "",
    "",
    "Fizz",
    "",
    "Buzz",
    "Fizz",
    "",
    "",
    "Fizz",
    "Buzz",
    "",
    "Fizz",
    "",
    "",
    "FizzBuzz",
    "",
    "",
    "Fizz",
    "",
    "Buzz"
  )

  expect_identical(fizzbuzz(20, show_numbers = FALSE), exp)
  ind <- exp == ""
  exp[ind] <- seq_len(20)[ind]
  expect_identical(fizzbuzz(20, show_numbers = TRUE), exp)

  expect_length(fizzbuzz(200), 200)
  expect_length(fizzbuzz_lazy(200), 200)

  expect_length(.fizzbuzz_vector, 1e6)
  expect_error(fizzbuzz_lazy(1e6 + 1), class = "simpleError")

  expect_error(fizzbuzz(NULL), class = "simpleError")
  expect_error(fizzbuzz(0), class = "simpleError")
  expect_error(fizzbuzz(-1), class = "simpleError")
})
