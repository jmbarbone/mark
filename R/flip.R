#' Flip
#'
#' Flip an object.
#'
#' @param x An object
#' @param by_row `TRUE`, flips by row, otherwise by column
#' @param keep_rownames Logical, if `TRUE` will not reset rownames; `NULL`
#' @param ... Additional arguments passed to methods
#' @return A vector of values, equal length of `x` that is reversed or a
#'   `data frame` with flipped rows/columns
#'
#' @examples
#' flip(letters[1:3])
#' flip(seq.int(9, -9, by = -3))
#' flip(head(iris))
#' flip(head(iris), keep_rownames = TRUE)
#' flip(head(iris), by_row = FALSE)
#'
#' @export

flip <- function(x, ...) {
  UseMethod("flip", x)
}

#' @export
#' @rdname flip
flip.default <- function(x, ...) {
  len <- length(x)

  if (len < 2) {
    return(x)
  }

  x[len:1L]
}

#' @export
#' @rdname flip
flip.matrix <- function(x, by_row = TRUE, keep_rownames = NULL, ...) {
  if (by_row) {
    rows <- nrow(x)
    dims <- dimnames(x)

    if (rows < 2) {
      return(x)
    }

    out <- x[rows:1, , drop = FALSE]
    rn <- dims[[1]]

    if (is.null(keep_rownames)) {
      keep_rownames <- !identical(rn, 1:rows)
    }

    if (!keep_rownames) {
      dims[[1]] <- rn
      dimnames(out) <- dims
    }

  } else {
    cols <- ncol(x)

    if (length(x) == 0L) {
      return(x)
    }

    out <- x[, cols:1L, drop = FALSE]
  }

  out
}

#' @export
#' @rdname flip
flip.data.frame <- function(x, by_row = TRUE, keep_rownames = NULL, ...) {
  if (by_row) {
    rows <- nrow(x)

    if (rows < 2) {
      return(x)
    }

    out <- x[rows:1, , drop = FALSE]
    rn <- attr(x, "row.names")

    if (is.null(keep_rownames)) {
      keep_rownames <- !identical(rn, 1:rows)
    }

    if (!keep_rownames) {
      attr(out, "row.names") <- rn
    }
  } else {
    cols <- ncol(x)

    if (!cols) {
      return(x)
    }

    out <- x[, cols:1L, drop = FALSE]
  }

  out
}
