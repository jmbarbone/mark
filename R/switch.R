#' Switch with a list of parameters
#'
#' Switch with a list of params
#'
#' @references
#' Original: https://stackoverflow.com/a/32835930/12126576
#'
#' @param x A single value
#' @param params A list of cases
#'
#' @export

switch_params <- function(x, params) {
  if (!is.list(params) || length(x) != 1L) {
    stop("params must be a list of a single length", call. = FALSE)
  }

  do.call(switch, c(x, params))
}
