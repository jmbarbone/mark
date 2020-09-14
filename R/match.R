#' Value Non-matching
#'
#' Non matching alternatives and supplementary functions.
#'   Contrast with [base::match()], [base::intersect()], and [base::`%in%`()]
#'
#' @inheritParams base::`%in%`
#' @export
#' @examples
#' 1:10 %in% c(1,3,5,9)
#' 1:10 %out% c(1,3,5,9)
#' letters[1:5] %wo% letters[3:7]
#' letters[1:5] %wi% letters[3:7]
#'
#' ## Note that setdiff() is very similar and typically makes more sense:
#'         c(1:6,7:2) %wo% c(3,7,12)  # -> keeps duplicates
#' setdiff(c(1:6,7:2),     c(3,7,12)) # -> unique values

`%out%` <- function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}

#' @rdname grapes-out-grapes
#' @export
`%wo%` <- function(x, table) {
  x[x %out% table]
}

#' @rdname grapes-out-grapes
#' @export
`%wi%` <- function(x, table) {
  x[match(table, x, nomatch = 0L)]
}

#' @rdname grapes-out-grapes
#' @export
no_match <- function(x, table) {
  all_na(match(x, table, nomatch = NA_integer_, incomparables = NULL))
}
