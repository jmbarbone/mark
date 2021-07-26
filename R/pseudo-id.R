#' Create an ID for a vector
#'
#' Transforms a vector into an integer of IDs.
#'
#' @param x A vector of values
#'
#' @returns A `pseudo_id` object where the `integer` value of the vector
#' correspond to the position of the unique values in the attribute `"uniques"`.
#' @examples
#' set.seed(42)
#' (x <- sample(letters, 10, TRUE))
#' (pid <- pseudo_id(x))
#' attr(pid, "uniques")[pid]
#'
#' @export
pseudo_id <- function(x) {
  UseMethod("pseudo_id", x)
}

#' @export
#' @rdname pseudo_id
pseudo_id.pseudo_id <- function(x) {
  x
}

#' @export
#' @rdname pseudo_id
pseudo_id.default <- function(x) {
  ux <- na_last(unique(x))
  make_pseudo_id(match(x, ux), ux)
}

#' @export
#' @rdname pseudo_id
pseudo_id.factor <- function(x) {
  lvl <- levels(x)
  m <- seq_along(lvl)[x]

  if (anyNA(m)) {
    if (!anyNA(lvl)) {
      lvl <- c(lvl, NA)
    }
    m[is.na(m)] <- length(lvl)
  }

  make_pseudo_id(m, lvl)
}

make_pseudo_id <- function(x, u) {
  struct(x, class = c("pseudo_id", "integer"), uniques = u)
}

na_last <- function(x) {
  if (anyNA(x)) {
    nas <- is.na(x)
    c(x[!nas], x[nas])
  } else {
    x
  }
}

.uniques <- function(x) {
  attr(x, "uniques")
}

`.uniques<-` <- function(x, value) {
  attr(x, "uniques") <- value
  x
}
