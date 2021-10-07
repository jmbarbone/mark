#' Value matching - Extensions
#'
#' Non matching alternatives and supplementary functions.
#'
#' @details
#' Contrast with [base::match()], [base::intersect()], and `%in%`
#' The functions of `%wi%` and `%wo%` can be used in lieu of `intersect()` and
#'   `setdiff()`.  The primary difference is that the base functions return only
#'   unique values, which may not be a desired behavior.
#'
#' @inheritParams base::`%in%`
#' @return
#' * `%out%`: A `logical` vector of equal length of `x`, `table`
#' * `%wo%`, `%wi%`: A vector of values of `x`
#' * `any_match()`, `no_match()`: `TRUE` or `FALSE`
#'
#' @export
#' @examples
#' 1:10 %in% c(1,3,5,9)
#' 1:10 %out% c(1,3,5,9)
#' letters[1:5] %wo% letters[3:7]
#' letters[1:5] %wi% letters[3:7]
#'
#' # base functions only return unique values
#'
#'           c(1:6,7:2) %wo% c(3,7,12)  # -> keeps duplicates
#'   setdiff(c(1:6,7:2),     c(3,7,12)) # -> unique values
#'
#'           c(1:6,7:2) %wi% c(3,7,12)  # -> keeps duplicates
#' intersect(c(1:6,7:2),     c(3,7,12)) # -> unique values
#'
#' @name match_ext

`%out%` <- function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}

#' @rdname match_ext
#' @export
`%wo%` <- function(x, table) {
  x[x %out% table]
}

#' @rdname match_ext
#' @export
`%wi%` <- function(x, table) {
  x[match(table, x, nomatch = 0L)]
}

#' @rdname match_ext
#' @export
no_match <- function(x, table) {
  !any(match(x, table, nomatch = 0L) != 0L)
}

#' @rdname match_ext
#' @export
any_match <- function(x, table) {
  any(match(x, table, nomatch = 0L) != 0L)
}
