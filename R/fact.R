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
#' @param x A vector of values
#' @return A `factor` vector of equal length of `x`
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
  attributes(out) <- list(levels = .uniques(out))
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
  out <- as.integer(x) + 1L
  nas <- is.na(x)
  out[nas] <- 3L
  levels(out) <- c("TRUE", "FALSE", if (any(nas)) NA)
  class(out) <- "factor"
  out
}

#' @rdname fact
#' @export
fact.factor <- function(x) {
  x
}

#' @rdname fact
#' @export
fact.pseudo_id <- function(x) {
  attributes(x) <- list(levels = as.character(.uniques(x)))
  class(x) <- "factor"
  x
}

#' Ordered
#'
#' As ordered
#'
#' @details
#' Simple implementation of `ordered`.  If `x` is `ordered` it is simply
#'   returned.  If `x` is a `factor` the `ordered` class is added.  Otherwise,
#'   `x` is made into a `factor` with [mark::fact()] and then the `ordered`
#'   class is added.
#'
#' @inheritParams fact
#' @export
#' @returns An `ordered` vector
as_ordered <- function(x) {
  UseMethod("as_ordered", x)
}

#' @rdname as_ordered
#' @export
as_ordered.default <- function(x) {
  as_ordered(fact(x))
}

#' @rdname as_ordered
#' @export
as_ordered.factor <- function(x) {
  class(x) <- c("ordered", "factor")
  x
}

#' @rdname as_ordered
#' @export
as_ordered.ordered <- function(x) {
  x
}


fact_remove_na <- function(x) {
  if (!is.factor(x)) {
    stop("x must be a factor", call. = FALSE)
  }

  levels(x) <- remove_na(levels(x))
  x
}
