#' Rounding by a specific interval.
#'
#' Rounds a number or vector of numbers by another
#'
#' @param x A number or vector to round.
#' @param by The number by which to round
#' @param method An option to explicitly specify automatic rounding, ceiling, or floor
#' @param include0 If `FALSE` replaces `0` with `by`
#' @return A vector of `doubles` of the same length of `x`
#'
#' @export
#'
#' @examples
#' x <- seq(1, 13, by = 4/3)
#'
#' cbind(
#'   x,
#'   by_1 = round_by(x, 1),
#'   by_2 = round_by(x, 2),
#'   by_3 = round_by(x, 3)
#' )

round_by <- function(x, by = 1, method = c("round", "ceiling", "floor"), include0 = TRUE) {
  res <- do.call(match_param(method), list(x / by)) * by

  if (!include0) {
    res[res == 0] <- by
  }

  res
}
