#' Merge lists
#'
#' Merge lists with different or intersecting names
#'
#' @param x,y Lists to merge
#' @param keep When matching names are found, from which object should the values be
#' retained; `"x"` retains values from `x`, `"y"` retains values from `y`.
#' @examples
#' x <- list(a = 1, b = 2, c = NULL)
#' y <- list(a = 2, c = 3)
#' # compared to:
#' utils::modifyList(x, y)
#' merge_list(x, y)
#' merge_list(x, y, keep = "y")
#' @export
merge_list <- function(x, y, keep = c("x", "y")) {
  keep <- match_param(keep)
  x <- x %||% list()
  y <- y %||% list()
  stopifnot(is.list(x), is.list(y))
  x <- remove_null(x)
  y <- remove_null(y)
  res <- c(x, y)[!duplicated(c(names(x), names(y)), fromLast = keep == "y")]
  res[order(names(res))]
}
