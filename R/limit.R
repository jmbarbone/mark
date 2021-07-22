#' Limit
#'
#' Limit a numeric vector by lower and upper bounds
#'
#' @param x A numeric vector
#' @param lower A lower limit (as `x < lower`)
#' @param upper An upper limit (as `x > higher`)
#'
#' @return The vector `x` with `lower` and `upper` as the minimum, maximum
#'   values
#'
#' @export
limit <- function(x, lower = min(x), upper = max(x)) {
  if (!is.numeric(lower) || length(lower) != 1L) {
    stop("lower must be a single numeric value", call. = FALSE)
  }

  if (!is.numeric(upper) || length(upper) != 1L) {
    stop("upper must be a single numeric value", call. = FALSE)
  }

  if (lower > upper) {
    stop("lower cannot be more than upper", call. = FALSE)
  }

  x[x < lower] <- lower
  x[x > upper] <- upper
  x
}
