#' Expands a vector
#'
#' Expands vector x by y
#'
#' @param x,y Vectors
#' @param expand Character switch to expand or keep only the values that
#'   intersect, all values in `x` or `y`, or retain all values found.
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
  # fmt: skip
  if (!is_named(x)) names(x) <- x
  # fmt: skip
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
    stop(class_error("must_be", x, "data.frame"))
  }

  if (no_length(new_index)) {
    stop(input_error("`new_index` cannot be NULL or 0 length"))
  }

  xi <- if (is.null(index)) {
    x[[1L]]
  } else if (index == "row.names") {
    attr(x, "row.names")
  } else {
    x[[index]]
  }

  if (anyNA(xi)) {
    warning(reindex_warning(which(is.na(xi))))
  }

  if (is.null(xi)) {
    stop(reindex_error())
  }

  ro <- expand_by(xi, new_index, expand = expand, sort = sort)

  nm <- names(ro)
  out <- x[nm, ]
  attr(out, "row.names") <- nm # nolint: object_name_linter
  out
}

# helpers -----------------------------------------------------------------

unique_name_check <- function(x) {
  # Checks that names are unique in the vector
  nm <- names(x) %||% x
  lens <- counts(nm)
  int <- lens > 1L

  if (any(int)) {
    warning(expand_by_warning(names(lens[int])))
    FALSE
  } else {
    TRUE
  }
}

# conditions --------------------------------------------------------------

expand_by_warning := condition(
  function(x) paste0("These names are duplicated:", toString(x)),
  type = "warning",
  exports = "expand_by"
)

reindex_warning := condition(
  function(x) {
    paste(
      ngettext(
        length(x),
        "NA value detected in index at position:",
        "NA values detected in index at positions:"
      ),
      collapse(x, sep = ", ")
    )
  },
  type = "warning",
  exports = "reindex",
  help = "NA values in index may cause errors with expansion

`reindex()` will not match on NA values but instead will return a row of NAs

```r
x <- data.frame(index = c(1:2, NA, 4:5), values = letters[1:5])
reindex(x, 'index', c(1, 2, 5))
#>   index values
#> 1     1      a
#> 2     2      b
#> 5     5      e

reindex(x, 'index', c(1, 2, 5, NA))
#>      index values
#> 1        1      a
#> 2        2      b
#> 5        5      e
#> <NA>    NA   <NA>
```
"
)

reindex_error := condition(
  "x[[index]] returned `NULL`",
  type = "error",
  exports = "reindex"
)

# terminal line
