#' Count observations by unique values
#'
#' Get counts or proportions of unique observations in a vector or columns in a
#'   `data.frame`
#'
#' @description
#' Variables will be return by the order in which they appear.  Even factors are
#'   shown by their order of appearance in the vector.
#'
#' There are 2 methods for counting vectors.  The `default` method uses
#'   `base::tabulate()` (the workhorse for `base::table()` with a call to
#'   `pseudo_id()` to transform all inputs into integers.  The `logical` method
#'    counts `TRUE`, `FALSE` and `NA` values, which is much quicker.
#'
#' @param x A vector or `data.frame`
#' @param ... Arguments passed to other methods
#' @param sort Logical, if `TRUE` will sort values (not counts) before
#'   returning. For factors this will sort by factor levels.  This has no effect
#'   for logical vectors, which already return in the order of `FALSE`, `TRUE`,
#'   `NA`.
#' @param cols A vector of column names or indexes
#'
#' @return A named vector of `integer`s or `double`s (for `counts`, and `props`,
#'   respectively) or `data.frame` with columns for each column chosen and the
#'   `.name` chosen for the summary
#'
#' @examples
#' x <- sample(1:5, 10, TRUE)
#' counts(x)
#' props(x)
#'
#' x <- quick_df(list(
#'   a = c("a", "c", "a", "c", "d", "b"),
#'   b = c("a", "a", "a", "c", "c", "b"),
#'   c = c("a", "a", "a", "c", "b", "b")
#' ))
#'
#' counts(x, "a")
#' counts(x, c("a", "b", "c"))
#' props(x, 2)
#' props(x, 1:3)
#'
#' props(c(1, 1, 3, NA, 4))
#' props(c(1, 1, 3, NA, 4), na.rm = TRUE)
#' @export
counts <- function(x, ...) {
  UseMethod("counts", x)
}

#' @export
counts.default <- function(x, sort = FALSE, ...) {
  x <- pseudo_id(x)
  u <- .uniques(x)
  out <- tabulate(x, length(u))
  names(out) <- na_last(u)

  if (sort) {
    return(sort_names(out, numeric = is.numeric(x)))
  }

  out
}

#' @export
counts.factor <- function(x, ...) {
  x <- fact(x)
  lvl <- levels(x)
  x <- seq_along(lvl)[x]
  n <- length(lvl)
  x[is.na(x)] <- n
  set_names(tabulate(x, n), lvl)
}

#' @export
counts.logical <- function(x, ...) {
  fs <- sum(!x, na.rm = TRUE)
  ts <- sum(x, na.rm = TRUE)
  ns <- sum(is.na(x), na.rm = TRUE)
  out <- set_names(c(fs, ts, ns), c(FALSE, TRUE, NA))
  out[out != 0L]
}

#' @param .name The name of the new column
#' @rdname counts
#' @export
counts.data.frame <- function(x, cols, sort = FALSE, ..., .name = "freq") {
  if (!is.character(cols)) {
    cols <- colnames(x)[cols]
  }

  out <-
    if (length(cols) > 1) {
      counts_n(x[, cols], sort = sort, name = .name)
    } else {
      vector2df(counts(x[[cols]], sort = sort), cols, .name %||% "freq")
    }

  remake_df(out, x[, cols, drop = FALSE])
}

#' @rdname counts
#' @export
props <- function(x, ...) {
  UseMethod("props", x)
}

#' @rdname counts
#' @export
#' @param na.rm If `TRUE` will remove NA values from proportions
props.default <- function(x, sort = FALSE, na.rm = FALSE, ...) { # nolint: object_name_linter, line_length_linter.
  res <- counts(x, sort = sort)

  n <- length(res)

  if (na.rm && is.na(names(res[n]))) {
    res[n] <- NA_real_
    x <- remove_na(x)
  }

  res / length(x)
}

#' @rdname counts
#' @export
props.data.frame <- function(
  x,
  cols,
  sort = FALSE,
  na.rm = FALSE, # nolint: object_name_linter.
  ...,
  .name = "prop"
) {
  if (!is.character(cols)) {
    cols <- colnames(x)[cols]
  }

  out <-
    if (length(cols) > 1) {
      values <- x[, cols, drop = FALSE]
      na_ind <- if (na.rm) which(!stats::complete.cases(values))
      props_n(values, sort = sort, name = .name, na_ind = na_ind)
    } else {
      vector2df(
        props(x[[cols]], sort = sort, na.rm = na.rm), cols, .name %||% "prop"
      )
    }

  remake_df(out, x[, cols, drop = FALSE])
}

#' Count N
#'
#' Employs `counts()` for combinations of multiple rows
#'
#' @param x A data.frame in which the combination of all columns present will
#'   be counted
#' @param name A name for the new column
#' @param sort Logical, if `TRUE` sorts the output; This will sort based
#' @noRd
counts_n <- function(x, name = "freq", sort = FALSE) {
  # Can I save the call to unique here?
  ints <- do.call(paste, c(lapply(x, pseudo_id), sep = "."))
  res <- counts(ints, sort = sort)
  len <- length(res)
  non_dupe <- !duplicated(ints, nmax = len)
  out <- x[non_dupe, ]
  attr(out, "row.names") <- seq_len(len) # nolint: object_name_linter.

  cn <- colnames(x)
  colnames(out) <- cn
  name <- name %||% "freq"
  out[[name]] <- res
  out
}

#' @rdname counts_n
#' @noRd
props_n <- function(
  x,
  sort = FALSE,
  na.rm = FALSE, # nolint: object_name_linter.
  name = "props",
  na_ind = NULL
) {
  res <- counts_n(x, name %||% "prop", sort = sort)
  n <- ncol(res) # maybe could use `name`?

  if (is.null(na_ind)) {
    big_n <- length(res[[n]])
  } else {
    res[[n]][na_ind] <- NA_real_
    big_n <- length(res[[n]][-na_ind])
  }

  res[[n]] <- res[[n]] / big_n
  res
}

remake_df <- function(new, old) {
  stopifnot(is.data.frame(new), is.data.frame(old))

  new_cn <- colnames(new)
  old_cn <- colnames(old)
  m <- remove_na(match(new_cn, old_cn))

  for (i in seq_along(m)) {
    n <- new_cn[i]
    o <- old_cn[m[i]]

    if (is.factor(old[[o]])) {
      new[[n]] <- match(new[[n]], levels(old[[o]]))
    }

    attributes(new[[n]]) <- attributes(old[[o]])
    class(new[[n]]) <- class(old[[o]])
  }

  colnames(new) <- make.unique(colnames(new), sep = "_")
  new
}
