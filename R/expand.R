#' Expands a vector
#'
#' Expands vector x by y
#'
#' @param x,y Vectors
#' @param expand Character switch to expand or keep only the values that
#'   intersect, all values in x or y, or retain all values found.
#' @param sort Logical, if `TRUE` will sort by names in output
#'
#' @examples
#' x <- letters[c(3:2, 5, 9)]
#' y <- letters[c(1:4, 8)]
#' expand_by(x, y, "x")
#' expand_by(x, y, "y")
#' expand_by(x, y, "intersect")
#' expand_by(x, y, "both")
#' @export
expand_by <- function(x, y, expand = c("x","y", "intersect", "both"), sort = FALSE) {
  expand <- match.arg(expand)
  if (!is_named(x)) names(x) <- x
  if (!is_named(y)) names(y) <- y

  nx <- names(x)
  ny <- names(y)

  stopifnot(unique_name_check(x), unique_name_check(y))

  out <- switch(
    expand,
    intersect = {
      mt <- match(ny, nx, 0L)
      x[mt]
    },
    x = {
      mt <- match(nx, ny, 0L) == 0L
      out <- x
      out[mt] <- NA
      out[!mt] <- x[!mt]
      out
    },
    y = {
      mt <- match(ny, nx, 0L) == 0L
      out <- y
      out[mt] <- NA
      out[!mt] <- x[!mt]
      nm <- names(out)
      out2 <- x[nm]
      names(out2) <- nm
      out2
    },
    both = {
      uq <- unique(c(nx, ny))
      out <- x[uq]
      ind <- match(uq, nx, 0L) == 0L
      names(out)[ind] <- uq[ind]
      out
    }
  )
  if (sort) out <- out[sort(names(out))]
  out
}


#' Reindex a data.frame
#'
#' Reindexes a data.frame with a reference
#'
#' @param x A data.frame
#' @param index The column name or number of an index to use; if `NULL` will assume the
#'   first column; a value of `row.names` will use `row.names(x)`
#' @param new_index A column vector of the new index value
#' @param expand Character switch to expand or keep only the values that
#'   intersect (none), all values in x or index, or retain all values found.
#' @param sort Logical, if `TRUE` will sort the rows in output
#'
#' @examples
#' iris1 <- head(iris, 5)
#' iris1$index <- 1:5
#' reindex(iris1, "index", seq(2, 8, 2))
#' reindex(iris1, "index", seq(2, 8, 2), expand = "both")
#'
#' # Using letters will show changes in rownames
#' iris1$index <- letters[1:5]
#' reindex(iris1, "index", letters[seq(2, 8, 2)])
#' reindex(iris1, "index", seq(2, 8, 2))
#' reindex(iris1, "index", seq(2, 8, 2), expand = "both")
#' @export
reindex <- function(x, index = NULL, new_index, expand = c("intersect", "both"),  sort = FALSE) {
  expand <- match.arg(expand)
  stopifnot("`x` must be a data.frame" = inherits(x, "data.frame"),
            !is.null(new_index),
            length(new_index) != 0L)

  xi <- if (is.null(index)) {
    x[[1L]]
  } else if (index == "row.names") {
    rownames(x)
  } else {
    x[[index]]
  }

  if (anyNA(xi)) {
    warning("NA values detected in index this may cause errors with expansion",
            call. = FALSE)
  }

  if (is.null(xi)) {
    stop("x[[index]] returned `NULL`", call. = FALSE)
  }

  ro <- expand_by(xi, new_index, expand = expand, sort = sort)

  nm <- names(ro)
  out <- x[nm, ]
  rownames(out) <- nm
  out
}


# FUNS --------------------------------------------------------------------

unique_name_check <- function(x) {
  # Checks that names are unique in the vector
  nm <- names(x)
  if (is.null(nm)) nm <- x

  lens <- sapply(split(nm, nm), length, USE.NAMES = TRUE)
  int <- lens > 1

  if (any(int)) {
    warning("These names are duplicated: ",
            paste0(names(lens[int]), collapse = ", "),
            call. = FALSE)
    invisible(FALSE)
  } else {
    invisible(TRUE)
  }
}
