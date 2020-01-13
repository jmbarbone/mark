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
#' round_by(sample(100, 100, replace = TRUE), by = 5)
#' round_by(seq(1, 20, .6), 2, "ceiling")
#' round_by(runif(10), .01, "floor")
#' round_by(runif(10) * 100, seq(10))

round_by <- function(x, by = 1, method = "round") {
  switch(match.arg(method, c("round", "ceiling", "floor")),
         round   = round(x / by) * by,
         floor   = mapply(function(x, by)   floor(x / by) * by, x = x, by = by, USE.NAMES = FALSE),
         ceiling = mapply(function(x, by) ceiling(x / by) * by, x = x, by = by, USE.NAMES = FALSE))
}
