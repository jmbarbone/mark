#' Merge lists
#'
#' Merge lists with different or intersecting names
#'
#' @param x,y Lists to merge
#' @param keep When matching names are found, from which object should the values be
#' retained; `"x"` retains values from `x`, `"y"` retains values from `y`.
#' @examples
#' merge_list(
#'   list(a = 1, b = 2, c = NULL),
#'   list(a = 2, c = 3)
#' )
#'
#' merge_list(
#'   list(a = 1, b = 2, c = NULL),
#'   list(a = 2, c = 3),
#'   keep = "y"
#' )
#' @export
merge_list <- function(x, y, keep = c("x", "y")) {
  keep <- match.arg(keep)
  x <- x %||% list()
  y <- y %||% list()
  stopifnot(is.list(x), is.list(y))
  x <- Filter(Negate(is.null), as.list(x))
  y <- Filter(Negate(is.null), as.list(y))
  c(x, y)[!duplicated(c(names(x), names(y)), fromLast = keep == "y")]
}
