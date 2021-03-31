#' Count observations by unique values
#'
#' Get counts or proportions of unique observations in a vector or columns in a
#'   data.frame
#'
#' @description
#' Variables will be return by the order in which they appear.  Even factors are
#'   shown by their order of appearance in the vector.
#'
#' There are 4 methods for counting vectors.  The `default` method uses a
#'   reworked version of `base::rle`.  The `logical` method counts `TRUE`,
#'   `FALSE` and `NA` values, which should be quicker than the other methods.
#'   The `character` creates a quick factor with and `split()`s the vector
#'   before using `lengths()`.  The `factor` method simply recodes the levels
#'   first.
#'
#' @param x A vector or data.frame
#' @param ... Arguments passed to other methods
#' @param sort Logical, if `TRUE` will sort values before returning. For factors
#'   this will sort by factor levels.  This has no effect for logical vectors,
#'   which already return in the order of `FALSE`, `TRUE`, `NA`.
#' @param cols A vector of column names or indexes
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
#' @export
counts <- function(x, ...) {
  UseMethod("counts", x)
}


#' @export
counts.default <- function(x, sort = FALSE, ...) {
  n <- length(x)

  if (n == 0L) {
    return(integer(0L))
  }

  sx <- sort(x, na.last = TRUE)
  y <- sx[-1L] != sx[-n]
  i <- c(which(y | is.na(y)), n)
  out <- diff(c(0L, i))
  names(out) <- sx[i]

  if (sort) {
    return(out)
  }

  out[match(unique(x), sx[i])]
}


#' @export
counts.logical <- function(x, ...) {
  fs <- sum(!x, na.rm = TRUE)
  ts <- sum(x, na.rm = TRUE)
  ns <- sum(is.na(x), na.rm = TRUE)
  out <- set_names0(c(fs, ts, ns), c(FALSE, TRUE, NA))
  out[out != 0L]
}

#' @export
counts.character <- function(x, sort = FALSE, ...) {
  ux <- unique(x)

  if (sort) {
    ux <- sort(ux)
  }

  n <- length(ux)

  if (n == 0L) {
    return(integer(0L))
  }

  fact <- factor(
    x,
    levels = ux,
    labels = ux,
    exclude = NULL,
    ordered = FALSE,
    nmax = n
  )

  lengths(split(x, fact), use.names = TRUE)
}

#' @export
counts.factor <- function(x, sort = FALSE, ...) {
  if (sort) {
    return(lengths(split(x, x)))
  }

  x <- levels(x)[x]
  counts.character(x)
}

#' @param .name The name of the new column
#' @rdname counts
#' @export
counts.data.frame <- function(x, cols, sort = FALSE, ..., .name = "freq") {
  if (!is.character(cols)) {
    cols <- colnames(x)[cols]
  }

  if (length(cols) > 1) {
    return(counts_n(x[, cols], sort = sort, name = .name))
  }

  vector2df(counts(x[[cols]], sort = sort), cols, .name %||% "freq")
}

#' @rdname counts
#' @export
props <- function(x, ...) {
  UseMethod("props", x)
}

#' @rdname counts
#' @export
props.default <- function(x, ...) {
  counts(x) / length(x)
}

#' @rdname counts
#' @export
props.data.frame <- function(x, cols, sort = FALSE, ..., .name = "prop") {
  if (!is.character(cols)) {
    cols <- colnames(x)[cols]
  }

  if (length(cols) > 1) {
    return(props_n(x[, cols], sort = sort, name = .name))
  }

  vector2df(props(x[[cols]], sort = sort), cols, .name %||% "prop")
}

#' Count N
#'
#' Employs `counts()` for combinations of multiple rows
#'
#' @param x A data.frame in which the combination of all columns present will
#'   be counted
#' @param name A name for the new column
#' @param sort Logical, if `TRUE` sorts the output; This will sort based
counts_n <- function(x, name = "freq", sort = FALSE) {

  # Can I save the call to unique here?
  ints <- do.call(paste, c(lapply(x, pseudo_id), sep = "."))
  res <- counts(ints, sort = sort)
  len <- length(res)
  non_dupe <- !duplicated(ints, nmax = len)
  out <- x[non_dupe, ]
  attr(out, "row.names") <- 1:len

  cn <- colnames(x)
  colnames(out) <- cn

  # TODO add warning
  name <- name %||% "freq"

  i <- 0L
  while (name %in% cn) {
    i <- i + 1L
    name <- sprintf("%s_%d", name, i)
  }

  out[[name]] <- res
  out
}

#' @rdname counts_n
props_n <- function(x, sort = FALSE, name = "props") {
  res <- counts_n(x, name %||% "prop", sort = sort)
  n <- ncol(res)
  res[[n]] <- res[[n]] / nrow(x)
  res
}
