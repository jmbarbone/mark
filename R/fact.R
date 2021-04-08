#' Factor
#'
#' Quickly create a factor
#'
#' @details
#' `fact()` can be about 5 times quicker than `factor()` or `as.factor()`
#'   as it doesn't bother sorting the levels for non-numeric data or have
#'   other checks or features.  It simply converts a vector to a factor with all
#'   unique values as levels with `NA`s included.
#'
#' @param x A vector
#' @export
fact <- function(x) {
  UseMethod("fact", x)
}

#' @rdname fact
#' @export
fact.default <- function(x) {
  stop("No fact method for class(es) ", collapse0(class(x), sep = ", "),
       call. = FALSE)
}

#' @rdname fact
#' @export
fact.character <- function(x) {
  out <- pseudo_id(x)
  attributes(out) <- list(levels = attr(out, "uniques"))
  class(out) <- "factor"
  out
}

#' @rdname fact
#' @export
fact.numeric <- function(x) {
  u <- sort.int(unique(x), method = "radix", na.last = TRUE)
  out <- match(x, u)
  attributes(out) <- list(levels = as.character(u))
  class(out) <- "factor"
  out
}

#' @rdname fact
#' @export
fact.logical <- function(x) {
  out <- as.integer(x)
  levels(out) <- c("TRUE", "FALSE", if (anyNA(x)) NA else NULL)
  class(out) <- "factor"
  out
}

#' @rdname fact
#' @export
fact.factor <- function(x) {
  x
}

fact_remove_na <- function(x) {
  if (!is.factor(x)) {
    stop("x must be a factor", call. = FALSE)
  }

  levels(x) <- remove_na(levels(x))
  x
}
