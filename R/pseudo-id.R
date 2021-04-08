#' Create an ID for a vector
#'
#' Transforms a vector into an integer of IDs.
#'
#' @param x A vector
#'
#' @examples
#'\dontrun{
#' set.seed(42)
#' (x <- sample(letters, 10, TRUE))
#' (pid <- pseudo_id(x))
#' attr(pid, "uniques")[pid]
#' }
#' @export
pseudo_id <- function(x) {
  UseMethod("pseudo_id", x)
}

#' @export
#' @rdname pseudo_id
pseudo_id.default <- function(x) {
  ux <- unique(x)
  m <- match(x, na_last(as.character(ux)))
  attr(m, "uniques") <- ux
  m
}

#' @export
#' @rdname pseudo_id
pseudo_id.factor <- function(x) {
  lvl <- levels(x)
  m <- seq_along(lvl)[x]

  if (anyNA(m) && !anyNA(lvl)) {
    lvl <- c(lvl, NA)
  }

  attr(m, "uniques") <- lvl
  m
}

na_last <- function(x) {
  if (anyNA(x)) {
    nas <- is.na(x)
    c(x[!nas], x[nas])
  } else {
    x
  }
}
