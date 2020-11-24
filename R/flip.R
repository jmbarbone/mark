#' Flip an object
#'
#' @param x An object
#' @param by_row `TRUE`, flips by row, otherwise by column
#' @param rownames `FALSE`, if `TRUE` will update the rownames
#' @param ... Additional arguments passed to methods
#'
#' @examples
#' flip(letters[1:3])
#' flip(seq.int(9, -9, by = -3))
#' flip(head(iris))
#' flip(head(iris), rownames = TRUE)
#' flip(head(iris), by_row = FALSE)
#'
#' @export

flip <- function(x, ...) {
  UseMethod("flip")
}

#' @export
#' @rdname flip
flip.default <- function(x, ...) {
  len <- length(x)

  if (!len) {
    stop("Object is of length > 0", call. = FALSE)
  }

  x[len:1L]
}

#' @export
#' @rdname flip
flip.data.frame <- function(x, by_row = TRUE, rownames = FALSE, ...) {
  if (by_row) {
    rows <- nrow(x)
    if (!rows) {
      stop("data.frame has no rows", call. = FALSE)
    }
    out <- x[rows:1L, ]
    if (rownames) {
      rownames(out) <- flip(rownames(out))
    }

  } else {
    cols <- ncol(x)
    if (!cols) {
      stop("data.frame has no columns", call. = FALSE)
    }
    out <- x[, cols:1L]
  }

  out
}
