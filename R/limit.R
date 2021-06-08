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
  stopifnot(
    "lower must be a single value" = is.numeric(lower) && length(lower) == 1L,
    "lower must be a single value" = is.numeric(upper) && length(upper) == 1L,
    "lower cannot be more than upper" = lower > upper
  )

  x[x < lower] <- lower
  x[x > upper] <- upper
  x
}
