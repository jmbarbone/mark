#' Rounding by a specific interval.
#'
#' Rounds a number or vector of numbers by another
#'
#' @param x A number or vector to round.
#' @param by The number by which to round
#' @param method An option to explicitly specify automatic rounding, ceiling, or floor
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

round_by <- function(x, by = 1, method = c("round", "ceiling", "floor")) {
  do.call(match_param(method), list(x / by)) * by
}
