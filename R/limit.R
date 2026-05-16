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
  if (!(is.numeric(lower) && is.numeric(upper))) {
    stop(class_error("must_be", c(lower, upper), "numeric"))
  }

  if (!(length(lower) == 1L && length(upper) == 1L && lower <= upper)) {
    stop(input_error(
      "`lower` and `upper` must be single numeric values with `lower <= upper`"
    ))
  }

  x[x < lower] <- lower
  x[x > upper] <- upper
  x
}
