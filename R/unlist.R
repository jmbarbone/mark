#' Unlist and squash
#'
#' Unlist without unique names; combine names for unique values
#'
#' @details
#' * [unlist0()] is much like [unlist()] expect that name are not made to be
#'   unique.
#' * [squash_vec()] works differently
#'
#' @param x A vector of values
#' @examples
#' x <- list(a = 1:3, b = 2, c = 2:4)
#' y <- c(a = 1, b = 1, c = 1, d = 2, e = 3, f = 3)
#'
#' # unlist0() doesn't force unique names
#' unlist(x)   # names: a1 a2 a3  b c1 c2 c3
#' unlist0(x)  # names: a a a  b c c c
#' unlist0(y)  # no change
#'
#' # squash_vec() is like the inverse of unlist0() because it works on values
#' squash_vec(x)
#' squash_vec(y)
#' @export
#' @returns
#' * [unlist0()]: a vector with the possibility of non-unique names
#' * [squash_vec()]: A vector of unique values and names
unlist0 <- function(x) {
  if (!is.list(x)) {
    return(x)
  }

  unlist(x, use.names = FALSE) %names% rep.int(names(x), lengths(x))
}

#' @rdname unlist0
#' @export
#' @param sep A separation for combining names
squash_vec <- function(x, sep = ".") {
  x <- unlist0(x)
  id <- pseudo_id(x, na_last = FALSE)
  nm <- names(x)
  squasher <- function(i) collapse(nm[i], sep = sep)
  .uniques(id) %names% vap_chr(split(seq_along(id), id), squasher)
}
