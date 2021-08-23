#' Unlist
#'
#' Unlist without unique names
#'
#' @param x A list
#' @examples
#' x <- list(
#'   a = 1:3,
#'   b = 2,
#'   c = 2:4
#' )
#'
#' unlist(x)   # names: a1 a2 a3  b c1 c2 c3
#' unlist0(x)  # names: a a a  b c c c
#' @export
unlist0 <- function(x) {
  stopifnot(is.list(x))
  unlist(x, use.names = FALSE) %names% rep.int(names(x), lengths(x))
}
