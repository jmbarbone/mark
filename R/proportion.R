#' Proportions
#'
#' Calculates a proportion from a vector or data.frame/matrix.
#'
#' @details
#' tapply(x, x, length) / length(x) can be speedier for smaller data sets and groups
#'   but slows down with more data or a greater number of groups.
#'
#' @param x A data.frame or a vector.
#' @param ... Additional arguments to be passed to methods.
#' @param col A character string of the column name which holds the groups
#'
#' @examples
#' proportion(iris, "Species")
#' proportion(iris$Species)
#'
#' @export

proportion <- function(x, ...) {
  UseMethod("proportion", x)
}

#' @export
#' @rdname proportion
proportion.default <- function(x, ...) {
  vapply(split(x, as_factor_unordered(x)), length, double(1)) / length(x)
}

#' @export
#' @rdname proportion
proportion.data.frame <- function(x, col, ...) {
  vector2df(proportion.default(x[[col]]), "col_name", "props")
}

#' @export
#' @rdname proportion
cum_prop <- function(x) {
  x <- cumsum(x)
  x / x[length(x)]
}
