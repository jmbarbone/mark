#' Multiple searching
#'
#' Multiple search pattern searches
#'
#' @param x Passed to [base::grepl()]
#' @param patterns A list or vector of patterns to search across `x`; if named
#'   value returned will be the name of the pattern -- otherwise the position.
#'   Pattern match reported will be the first in the list that is found
#' @param ... Additional arguments passed to [base::grepl()]
#' @param simplify if `FALSE` will return a list of all matches, otherwise the
#'   first match found
#'
#' @return
#' The name or position of the pattern that is matched
#'
#' @examples
#' x <- c("apple", "banana", "lemon")
#' multi_grepl(x, c("a" = "^[ab]", "b" = "lem"))
#' multi_grepl(x, c("a" = "^[ab]", "b" = "q"))                   # lemon not matches on either
#' multi_grepl(x, c("a" = "^[ab]", "b" = "e"))                   # apple matches "a" before "b"
#' multi_grepl(x, c("a" = "^[ab]", "b" = "e"), simplify = FALSE) # shows all matches
#' multi_grepl(x, c("^[ab]", "e"))                               # returned as positions
#' multi_grepl(x, c("^[ab]", "e"), simplify = FALSE)
#'
#' @export

multi_grepl <- function(x, patterns, ..., simplify = TRUE) {
  nm <- names(patterns)
  null_names <- is.null(nm)

  if (null_names) {
    names(patterns) <- seq_along(patterns)
  }

  evals <- sapply(patterns, grepl, x, ...)
  out <- apply(evals, 1, which_no_0, .simplify = simplify)

  if (null_names) {
    out <- if (is.list(out)) {
      lapply(out, as.integer)
    } else {
      as.integer(out)
    }
  }

  out
}

#' @export
#' @rdname multi_grepl
multi_grep <- multi_grepl

which_no_0 <- function(x, .simplify = TRUE) {
  res <- names(which(x))

  if (no_length(res)) {
    return(NA)
  }

  if (.simplify) {
    return(res[1])
  }

  res
}

