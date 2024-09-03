#' Fizz Buzz
#'
#' For when someone asked you to do something you've done before, you can argue
#' that the quickest way to do it is to just take the work someone else did and
#' utilize that.  No reason to reinvent the wheel.
#'
#' @details
#' Multiples of `3` are shown as `"Fizz"`; multiples of `5` as `"Buzz"`;
#'   multiple of both (i.e., `15`) are `"FizzBuzz"`.
#' `fizzbuzz_lazy()` subsets the `.fizzbuzz_vector` object, which is a solution
#'   with default parameters up to `1e6`
#'
#' @param n The number of numbers
#' @param show_numbers If `TRUE` shows no
#' @export
#' @return A `character` vector of `1, 2, Fizz, 3, Buzz`, etc
#'
#' @examples
#' fizzbuzz(15)
#' fizzbuzz(30, show_numbers = FALSE)
#' cat(fizzbuzz(30), sep = "\n")
#'
#' \donttest{
#' # show them how fast your solution is:
#' if (package_available("bench")) {
#'   bench::mark(fizzbuzz(1e5), fizzbuzz_lazy(1e5))
#' }
#' }

fizzbuzz <- function(n, show_numbers = TRUE) {
  stopifnot(n >= 1)

  x <- 1:n

  out <- if (show_numbers) {
    as.character(x)
  } else {
    character(n)
  }

  three <- x %% 3 == 0
  five <- x %% 5 == 0

  out[three] <- "Fizz"
  out[five] <- "Buzz"
  out[three & five] <- "FizzBuzz"

  out
}

#' @rdname fizzbuzz
#' @export
fizzbuzz_lazy <- function(n) {
  stopifnot(n <= 1e6)
  .fizzbuzz_vector[1:n]
}

#' @rdname fizzbuzz
#' @export
.fizzbuzz_vector <- fizzbuzz(1e6)
