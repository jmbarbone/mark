#' Character to factor
#'
#' Converts characters to factors
#'
#' @param x A vector of characters
#' @param n The limit to the number of unique values for the factor
#' @export
char2fact <- function(x, n = 5) {
  UseMethod("char2fact", x)
}

#' @rdname char2fact
#' @export
char2fact.default <- function(x, n = 5) {
  stop("char2fact does not support class ", class(x), call. = FALSE)
}

#' @rdname char2fact
#' @export
char2fact.character <- function(x, n = 5) {
  id <- pseudo_id(x)

  if (length(.uniques(id)) <= n) {
    x <- fact(id)
  }

  x
}

#' @rdname char2fact
#' @export
char2fact.factor <- function(x, n = 5) {
  x
}

#' @rdname char2fact
#' @export
char2fact.data.frame <- function(x, n = 5) {
  for (i in which(vap_lgl(x, is.character))) {
    x[[i]] <- char2fact(x[[i]], n)
  }

  x
}
