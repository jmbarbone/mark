#' Proportions
#'
#' Calculates a proportion from a vector or data.frame/matrix.
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
  if (!inherits(x, "factor")) {
    x <- as_factor_unordered(x)
  }

  vap_dbl(split(x, x), length, .nm = TRUE) / length(x)
}

#' @export
#' @rdname proportion
proportion.data.frame <- function(x, col, ...) {
  cn <- colnames(x)
  is_in <- col %in% cn

  stopifnot("Column name not found" = any(is_in),
            "Multiple matches found" = sum(is_in) == 1L)

  props <- proportion.default(x[[col]])
  vector2df(props, col, "prop")
}
