#' Paste combine
#'
#' Paste and combine
#'
#' @param x,y,... Vectors to paste and/or combine
#' @param collate Logical; `TRUE` prints out combinations in order of the first
#'   vector elements then the next; otherwise reversed (see examples)
#' @param sep A character string to separate terms
#' @export
#'
#' @name utils-paste
#'
#' @examples
#' x <- letters[1:5]
#' y <- 1:3
#' z <- month.abb[c(1, 12)]
#' paste_combine(x, y)
#' paste_combine(x, y, z)
#' paste_combine(x, y, z, sep = ".")
#' paste_combine(x, y, sep = "_")
#' paste_combine(x, y, collate = FALSE)

paste_c <- function(x, y, collate = TRUE, sep = "") {
  warning("Use `jordan::paste_combine()` instead", call. = FALSE)
  paste_combine(x, y, collate = collate, sep = sep)
}


#' @rdname utils-paste
#' @export
paste_combine <- function(..., collate = TRUE, sep = "") {
  ls <- list(...)
  n <- length(ls)
  stopifnot("length of ... must be at least 2" = n >= 2)

  out <- do_paste_combine(ls[[1]], ls[[2]], collate = collate, sep = sep)

  if (n == 2) {
    return(out)
  }

  for (i in 2:n) {
    out <- do_paste_combine(out, ls[[i]], collate = collate, sep = sep)
  }

  out
}

do_paste_combine <- function(x, y, collate = TRUE, sep = "") {
  out <- outer(x, y, FUN = paste, sep = sep)
  if (collate) {
    out <- apply(out, 1, c)
  }
  as.vector(out, "character")
}

#' @rdname utils-paste
#' @export
collapse <- function(..., sep = " ") {
  paste(..., sep = NULL, collapse = sep)
}

#' @rdname utils-paste
#' @export
collapse0 <- function(...) {
  collapse(..., sep = "")
}
