#' NA at positions
#'
#' Convert specific values to NA
#'
#' @description
#' Converts select elements of a vector into `NA`s
#'
#' This is how the end results are
#' * `NA_at` expects suitable index values (`x[y] <- NA`)
#' * `NA_if` expects some values to match on (`x[x %in% y] <- NA`)
#'
#' @param x A vector of values
#' @param y `NA_at()`: a vector of suitable index values.  `NA_if()`: a vector
#'   of values to `match()` on.  Either can also accepted a function
#'   which takes the vector `x` as its first argument and returns a valid `y`
#' @param ... Additional values passed to `y` (if `y` is a function)
#'
#' @seealso Inspired by `dplyr::na_if`
#' @export
NA_at <- function(x, y, ...) {
  nx <- length(x)
  if (nx == 0L) {
    return(x)
  }

  if (is.function(y)) {
    FUN <- match.fun(y)
    y <- FUN(x, ...)
  } else {
    if (length(y) > nx) {
      stop("y cannot be longer than x", call. = FALSE)
    }
  }

  x[y] <- NA
  x
}

#' @rdname NA_at
#' @export
NA_if <- function(x, y, ...) {
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
