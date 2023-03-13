#' Sort by
#'
#' Sort an object by another object
#'
#' @param x A vector
#' @param by Another vector
#' @param ... Additional arguments passed to `base::order()`
#' @return The values of `x`, resorted
#'
#' @examples
#' l3 <- letters[1:3]
#' sort_by(l3, c(3, 2, 1))
#' # make a factor object with the reversed order
#' f <- factor(l3, levels = rev(l3))
#' sort_by(f, l3)
#' sort_by(1:3, rev(l3))
#'
#' @export
sort_by <- function(x, by, ...) {
  if (!is_atomic0(x) || !is_atomic0(by)) {
    stop(cond_sort_by_atomic())
  }

  x[order(by, ...)]
}

cond_sort_by_atomic <- function() {
  new_condition("`x` and `by` must be atomic vectors", "sort_by_atomic")
}
