#' Count observations by unique values
#'
#' Get counts or proportions of unique observations in a vector or columns in a
#'   data.frame
#'
#' @description
#' Variables will be return by the order in which they appear
#'
#' @param x A vector or data.frame
#' @param cols A vector of column names or indexes
#' @param ... Arguments passed to other methods
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
  UseMethod("counts")
}

#' @export
counts.default <- function(x, ...) {
  n <- length(x)
  sx <- sort(x, na.last = TRUE)
  y <- sx[-1L] != sx[-n]
  i <- c(which(y | is.na(y)), n)
  out <- diff(c(0L, i))
  names(out) <- sx[i]
  out[match(unique(x), sx[i])]
}

# Special cases that are much quicker for characters

#' @export
counts.logical <- function(x, ...) {
  fs <- sum(!x, na.rm = TRUE)
  ts <- sum(x, na.rm = TRUE)
  ns <- sum(is.na(x), na.rm = TRUE)
  out <- set_names0(c(fs, ts, ns), c(FALSE, TRUE, NA))
  out[out != 0L]
}

#' @export
counts.character <- function(x, ...) {
  ux <- unique(x)

  fact <- factor(
    x,
    levels = ux,
    labels = ux,
    exclude = NULL,
    ordered = FALSE,
    nmax = length(ux)
  )

  lengths(split(x, fact), use.names = TRUE)
}

#' @export
counts.factor <- function(x, ...) {
  x <- levels(x)[x]
  counts.character(x)
}

#' @rdname counts
#' @export
counts.data.frame <- function(x, cols, ...) {
  if (!is.character(cols)) {
    cols <- colnames(x)[cols]
  }

  if (length(cols) > 1) {
    return(counts_n(x[, cols]))
  }

  vector2df(counts(x[[cols]]), cols, "freq")
}

#' @rdname counts
#' @export
props <- function(x, ...) {
  UseMethod("props")
}

#' @rdname counts
#' @export
props.default <- function(x, ...) {
  counts(x) / length(x)
}

#' @rdname counts
#' @export
props.data.frame <- function(x, cols, ...) {
  if (!is.character(cols)) {
    cols <- colnames(x)[cols]
  }

  if (length(cols) > 1) {
    return(props2(x[, cols]))
  }

  vector2df(props(x[[cols]]), cols, "prop")
}

#' Count N
#'
#' Employs `counts()` for combinations of multiple rows
#'
#' @param x A data.frame in which the combination of all columns present will
#'   be counted
#' @param name A name for the new column
counts_n <- function(x, name = "freq") {
  ints <- do.call(paste, c(lapply(x, pseudo_id), sep = "."))
  res <- counts(ints)
  len <- length(res)
  non_dupe <- !duplicated(ints, nmax = len)
  out <- x[non_dupe, ]
  attr(out, "row.names") <- 1:len

  cn <- colnames(x)
  colnames(out) <- cn

  # TODO add warning
  i <- 0L
  while (name %in% cn) {
    i <- i + 1L
    name <- sprintf("%s_%d", name, i)
  }

  out[[name]] <- res
  out
}

props2 <- function(x) {
  res <- counts_n(x, "prop")
  n <- ncol(res)
  res[[n]] <- res[[n]] / nrow(x)
  res
}
