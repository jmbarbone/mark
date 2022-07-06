#' Median (Q 50)
#'
#' Median as the 50th quantile with an option to select quantile algorithm
#'
#' @details
#' `q50` is an alias for `median2`
#'
#' @inheritParams stats::quantile
#' @return See `stats::quantile()`
#'
#' @examples
#' set.seed(42)
#' x <- rnorm(100)
#' median(x)            # 0.08979677
#' median2(x, type = 7) # 0.08979677 - default type is 7
#' median2(x, type = 3) # 0.08976065
#'
#' @export
#' @seealso [stats::quantile()]

median2 <- function(x, type = 7, na.rm = FALSE) {
  stats::quantile(x, probs = .5, type = type, na.rm = na.rm, names = FALSE)
}

#' @export
#' @rdname median2
q50 <- median2

#' Range 2
#'
#' Employs `min()` and `max()`.  However, [base::range()], there is no argument
#'   for removing `Inf` values.
#'
#' @param x A numeric (or character) vector (see Note in [base::min])
#' @param na.rm Logical, if `TRUE` removes missing values
#' @return A `numeric` vector of length 2 of the minimum and maximum values,
#'   respectively
#'
#' @examples
#' \donttest{
#' x <- rep(1:1e5, 100)
#' system.time(rep(range(x),  100))
#' system.time(rep(range2(x), 100))
#' x[sample(x, 1e5)] <- NA
#'
#' system.time(rep(range(x, na.rm = TRUE), 100))
#' system.time(rep(range2(x, na.rm = TRUE), 100))
#' }
#' @export
range2 <- function(x, na.rm = FALSE) {
  c(min(x, na.rm = na.rm), max(x, na.rm = na.rm))
}
