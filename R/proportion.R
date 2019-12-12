#' Calculate a proportion
#'
#' Calculates a proportion!
#'
#' @param x A data.frame or a vector.
#' @param ... Additional arguments to be passed to methods.
#' @import stats
#' @export

proportion <- function(x, ...) {
  UseMethod("proportion")
}

proportion.tbl <- function(x, column) {
  col <- as.character(substitute(column))

  groups <- .my_sort(x[[col]])
  props <- vapply(groups, function(x) mean(x[[col]]), double(1), USE.NAMES = T)

  tibble::enframe(props, col, "prop")
}

proportion.data.frame <- function(x, column) {
  col <- as.character(substitute(column))

  groups <- .my_sort(x[[col]])
  props <- vapply(groups, function(x) mean(x[[col]]), double(1), USE.NAMES = F)

  data.frame(col = groups,
             prop = props)
}

.my_sort <- function(x) UseMethod(".my_sort")
.my_sort.factor <- function(x) unique(x)
.my_sort.character <- function(x) sort(unique(x))
.my_sort.numeric <- function(x) sort(unique(x))
