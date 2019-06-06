#' Repeat each
#'
#' Repeats each value in order they are passed rather than
#'
#' @param .x A vector of values to repeat
#' @param times The number of times to repeat each value in in `.x`
#' @return a `vector` with the values repeated in the order in which they were called
#' @export

rep_each <- function(.x, times = 1)
{
  UseMethod("rep_each")
}

rep_each.character <- function(.x, times = 1)
{
  as.vector(vapply(.x, rep, FUN.VALUE = character(times), times = times))
}

rep_each.numeric <- function(.x, times = 1)
{
  as.vector(vapply(.x, rep, FUN.VALUE = numeric(times), times = times))
}

rep_each.integer <- function(.x, times = 1)
{
  as.vector(vapply(.x, rep, FUN.VALUE = integer(times), times = times))
}
