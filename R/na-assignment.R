#' NA at positions
#'
#' Convert specific values to NA
#'
#' @description
#' Converts select elements of a vector into `NA`s
#'
#' This is how the end results are
#' * `NA_at` and `NA_if` require a suitable index value (`x[y] <- NA`)
#'   * `NA_at` expects `y` (or the result of function `y`) to be `integers`
#'   * `NA_if` expects `y` (or the result of function `y`) to be `logical`
#' * `NA_in` and `NA_out` expect some values to match on
#'   * `NA_in` checks `x[x %in% y] <- NA`
#'   * `NA_out` checks `x[x %out% y] <- NA` (see [mark::match_ext])
#'
#' @param x A vector of values
#' @param y Either a suitable value (see `Details`) or a function which accepts
#'   `x` as its first parameter and can return suitable values
#' @param ... Additional values passed to `y` (if `y` is a function)
#' @return `x` with assigned `NA` values
#'
#' @seealso Inspired by [dplyr::na_if()]
#'
#' @examples
#' let <- ordered(letters[1:5])
#' NA_at(let, c(1, 3, 5))   # [1] <NA> b    <NA> d    <NA>
#' NA_if(let, let <= "b")   # [1] <NA> <NA> c    d    e
#' NA_in(let, c("a", "c"))  # [1] <NA> b    <NA> d    e
#' NA_out(let, c("a", "c")) # [1] a    <NA> c    <NA> <NA>
#'
#' @name na_assignments
NULL

#' @rdname na_assignments
#' @export
NA_at <- function(x, y, ...) {
  nx <- length(x)

  if (nx == 0L) {
    return(x)
  }

  if (is.function(y)) {
    FUN <- match.fun(y)
    y <- FUN(x, ...)
  }

  stopifnot(
    "y must be a vector of integers" = isTRUE(all.equal(y, as.integer(y))),
    "values of y must not be greater than length of x" = max(y, na.rm = TRUE) <= nx,
    "length of y must not be greater than length of x" = length(y) <= nx
  )

  x[y] <- NA
  x
}

#' @rdname na_assignments
#' @export
NA_if <- function(x, y, ...) {
  nx <- length(x)

  if (nx == 0L) {
    return(x)
  }

  if (is.function(y)) {
    FUN <- match.fun(y)
    y <- FUN(x, ...)
  }

  stopifnot(
    "y must be the same length as x" = length(y) == nx,
    "y must be logical" = is.logical(y)
  )

  x[y] <- NA
  x
}

#' @rdname na_assignments
#' @export
NA_in <- function(x, y, ...) {
  if (length(x) == 0L) {
    return(x)
  }

  if (is.function(y)) {
    FUN <- match.fun(y)
    y <- FUN(x, ...)
  }

  x[x %in% y] <- NA
  x
}

#' @rdname na_assignments
#' @export
NA_out <- function(x, y, ...) {
  if (length(x) == 0L) {
    return(x)
  }

  if (is.function(y)) {
    FUN <- match.fun(y)
    y <- FUN(x, ...)
  }

  x[x %out% y] <- NA
  x
}
