#' Merge lists
#'
#' Merge lists with different or intersecting names
#'
#' @param x,y Lists to merge
#' @param keep When matching names are found, from which object should the
#'   values be retained; `"x"` retains values from `x`, `"y"` retains values
#'   from `y`.
#' @param null Method for handling `NULL` values.  When two values are passed,
#'   they will be applied to `x` and `y` respectively.  When a single value is
#'   passed, it will be applied to both `x` and `y`.
#' * `"ignore"`: `NULL` values are ignored.  When passes to `x`, the `NULL`
#' values will be retained if they are not overridden by `y`.
#' * `"drop"`: `NULL` values are dropped from merge and will not appear in the
#'  output.
#' * `"keep"`: `NULL` values are retained in the output and can override other
#'  values.
#' @examples
#' x <- list(a = 1, b = 2,    c = NULL, d = NULL)
#' y <- list(a = 2, b = NULL, c = 3)
#'
#' # compared to:
#' utils::modifyList(x, y)
#' utils::modifyList(x, y, keep.null = TRUE)
#'
#' merge_list(x, y)
#' merge_list(x, y, keep = "y")
#' merge_list(x, y, null = "drop")
#' @export
merge_list <- function(
    x,
    y,
    keep = c("x", "y"),
    null = c("ignore", "drop", "keep")[1:2]
) {
  if (length(null) == 1L) {
    null <- c(null, null)
  }

  stopifnot(length(null) == 2)
  keep <- match_param(keep)
  x <- x %||% list()
  y <- y %||% list()
  xx <- x
  stopifnot(is.list(x), is.list(y))
  x <- switch(
    null[1],
    keep = x,
    ignore = remove_null(x),
    drop = remove_null(x)
  )

  y <- switch(
    null[2],
    keep = y,
    ignore = remove_null(y),
    drop = remove_null(y)
  )

  res <- c(x, y)[!duplicated(c(names(x), names(y)), fromLast = keep == "y")]

  if (null[1] == "ignore") {
    return(merge_list(
      x = xx[names(xx) %out% names(res)],
      y = res,
      null = c("keep", "ignore")
    ))
  }

  res[order(names(res))]
}
