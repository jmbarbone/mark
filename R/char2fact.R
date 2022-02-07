#' Character to factor
#'
#' Converts characters to factors
#'
#' @param x A vector of characters
#' @param n The limit to the number of unique values for the factor
#' @seealso #' @seealso [fact2char()]
#' @family factors
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

#' Factor to character
#'
#' Convert factor columns to characters in a `data.frame`
#'
#' @param data A `data.frame`
#' @param threshold A threshold for the number of levels to be met/exceeded for
#'   transforming into a character
#' @returns The `data.frame` `data` with factors converted by the rule above
#' @seealso [char2fact()]
#' @family factors
#' @export
fact2char <- function(data, threshold = 10) {
  stopifnot(is.data.frame(data))
  # for factors with more than threshold levels, convert back to character
  bad <- lengths(lapply(data, levels)) >= threshold
  data[bad] <- lapply(data[bad], as.character)
  data
}
