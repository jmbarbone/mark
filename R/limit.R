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
    length(lower) == 1L,
    length(upper) == 1L,
    is.numeric(lower),
    is.numeric(upper),
    lower <= upper
  )

  x[x < lower] <- lower
  x[x > upper] <- upper
  x
}
