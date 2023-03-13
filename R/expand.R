#' Expands a vector
#'
#' Expands vector x by y
#'
#' @param x,y Vectors
#' @param expand Character switch to expand or keep only the values that
#'   intersect, all values in x or y, or retain all values found.
#' @param sort Logical, if `TRUE` will sort by names in output
#'
#' @return A vector with expanded
#'
#' @examples
#' x <- letters[c(3:2, 5, 9)]
#' y <- letters[c(1:4, 8)]
#' expand_by(x, y, "x")
#' expand_by(x, y, "y")
#' expand_by(x, y, "intersect")
#' expand_by(x, y, "both")
#' @export
expand_by <- function(
    x,
    y,
    expand = c("x", "y", "intersect", "both"),
    sort = FALSE
) {
  expand <- match_param(expand)
  if (!is_named(x)) names(x) <- x
  if (!is_named(y)) names(y) <- y

  nx <- names(x)
  ny <- names(y)

  if (!unique_name_check(x)) {
    stop("unique name check failed for x", call. = FALSE)
  }

  if (!unique_name_check(y)) {
    stop("unique name check failed for y", call. = FALSE)
  }

  out <- switch(
    expand,
    intersect = {
      x[match(ny, nx, 0L)]
    },
    x = {
      mt <- nx %out% ny
      out <- x
      out[mt] <- NA
      out[!mt] <- x[!mt]
      out
    },
    y = {
      mt <- ny %out% nx
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
      ind <- uq %out% nx
      names(out)[ind] <- uq[ind]
      out
    }
  )

  if (sort) {
    return(sort_names(out))
  }

  out
}

#' Reindex a data.frame
#'
#' Reindexes a data.frame with a reference
#'
#' @param x A data.frame
#' @param index The column name or number of an index to use; if `NULL` will
#'   assume the first column; a value of `row.names` will use `row.names(x)`
#' @param new_index A column vector of the new index value
#' @param expand Character switch to expand or keep only the values that
#'   intersect (none), all values in x or index, or retain all values found.
#' @param sort Logical, if `TRUE` will sort the rows in output
#'
#' @return A `data.frame` with rows of `index`
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
reindex <- function(
    x,
    index = NULL,
    new_index,
    expand = c("intersect", "both"),
    sort = FALSE
) {
  expand <- match_param(expand)

  if (!inherits(x, "data.frame")) {
    stop("`x` must be a data.frame", call. = FALSE)
  }

  if (no_length(new_index)) {
    stop("new_index must not be NULL or 0 length", call. = FALSE)
  }

  xi <- if (is.null(index)) {
    x[[1L]]
  } else if (index == "row.names") {
    attr(x, "row.names")
  } else {
    x[[index]]
  }

  if (anyNA(xi)) {
    warning(cond_reindex_na())
  }

  if (is.null(xi)) {
    stop(cond_reindex_index())
  }

  ro <- expand_by(xi, new_index, expand = expand, sort = sort)

  nm <- names(ro)
  out <- x[nm, ]
  attr(out, "row.names") <- nm # nolint: object_name_linter
  out
}

# FUNS --------------------------------------------------------------------

unique_name_check <- function(x) {
  # Checks that names are unique in the vector
  nm <- names(x) %||% x
  lens <- counts(nm)
  int <- lens > 1

  if (any(int)) {
    warning("These names are duplicated: ",
            collapse0(names(lens[int]), sep = ", "),
            call. = FALSE)
    return(invisible(FALSE))
  }

  invisible(TRUE)
}

# conditions --------------------------------------------------------------

cond_reindex_na <- function() {
  new_condition(
    "NA values detected in index this may cause errors with expansion",
    "reindex_na",
    type = "warning"
  )
}

cond_reindex_index <- function() {
  new_condition("x[[index]] returned `NULL`", "reindex_index")
}

# terminal line
