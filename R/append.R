# should this only accept a single value for each?
append0 <- function(x, values, pos = NULL, ...) {
  UseMethod("append0")
}

# maybe values should be a named list?

#' @export
append0.data.frame <- function(x, values, pos = NULL, ...) {
  quick_df(append0(as.list(x), values = values, pos = pos, expand = TRUE))
}

#' @export
append0.list <- function(x, values, pos = NULL, expand = FALSE, ...) {
  if (!is.list(values)) {
    values <- list(values)
  }

  if (expand) {
    n <- unique(lengths(x))

    if (length(n) > 1) {
      warning(cond_append_expand())
      n <- max(n)
    }

    for (i in seq_along(values)) {
      values[[i]] <- rep_len(values[[i]], n)
    }
  }

  if (!is.list(values)) {
    values <- list(values)
  }

  len <- length(x)

  if (is.null(pos) || pos > len) {
    return(c(x, values))
  }

  if (pos == 1L) {
    return(c(values, x))
  }

  c(x[0L:(pos - 1L)], values, x[pos:len])
}

#' @export
append0.default <- function(x, values, pos = NULL, ...) {
  if (is.null(pos)) {
    return(c(x, values))
  }

  if (pos == 1L) {
    return(c(values, x))
  }

  n <- length(x)
  pos <- min(pos, n)
  c(x[1L:(pos - 1L)], values, x[pos:n])
}


# conditions --------------------------------------------------------------

cond_append_expand <- function() {
  new_condition("expanding to the largest n", "append0", type = "warning")
}
