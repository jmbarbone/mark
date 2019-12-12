#' Calculate a proportion
#'
#' Calculates a proportion!
#'
#' @param x A data.frame or a vector.
#' @param ... Additional arguments to be passed to methods.
#'
#' @examples
#' proportion(iris, Species)
#'
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

proportion.data.frame <- function(x, col) {

  if(is.name(substitute(col))) {
    vals <- eval(substitute(col), x, enclos = parent.frame())
    col_name <- deparse(substitute(col))
  } else {
    vals <- x[[col]]
    col_name <- col
  }

  groups <- unique(vals)

  res <- data.frame(
    col_name = groups,
    props = vapply(groups, function(.x) mean(.x == vals), double(1))
  )

  names(res)[1] <- col_name
  res
}

.my_sort <- function(x) UseMethod(".my_sort")
.my_sort.factor <- function(x) unique(x)
.my_sort.default <- function(x) sort(unique(x))
