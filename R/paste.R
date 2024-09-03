#' Paste combine
#'
#' Paste and combine
#'
#' @param x,y,... Vectors to paste and/or combine
#' @param collate Logical; `TRUE` prints out combinations in order of the first
#'   vector elements then the next; otherwise reversed (see examples)
#' @param sep A character string to separate terms
#' @return A `character` vector
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
#' collapse0(list(1:3, letters[1:3]), 5:7, letters[5:7])
#' collapse0(1:3, letters[5:7], sep = "_")

paste_c <- function(x, y, collate = TRUE, sep = "") {
  .Deprecated("paste_combine")
  paste_combine(x, y, collate = collate, sep = sep)
}

#' @rdname utils-paste
#' @export
paste_combine <- function(..., collate = TRUE, sep = "") {
  ls <- rlang::list2(...)
  n <- length(ls)

  if (n < 2) {
    stop(cond_paste_combine_length())
  }

  out <- do_paste_combine(ls[[1]], ls[[2]], collate = collate, sep = sep)

  if (n == 2) {
    return(out)
  }

  for (i in 3:n) {
    out <- do_paste_combine(out, ls[[i]], collate = collate, sep = sep)
  }

  out
}

#' @rdname utils-paste
#' @export
collapse0 <- function(..., sep = "") {
  ls <- rlang::list2(...)
  paste0(unlist(ls), collapse = sep)
}

# reduces outer function down to key elements
do_paste_combine <- function(x, y, collate = TRUE, sep = "") {
  xn <- length(x)
  yn <- length(y)

  y_reps <- rep(y, rep.int(xn, yn))

  out <- paste(
    rep(x, times = ceiling(xn / length(y_reps))),
    y_reps,
    sep = sep
  )

  if (!collate) {
    return(out)
  }

  dim(out) <- c(xn, yn)
  as.vector(apply(out, 1, c), "character")
}

# conditions --------------------------------------------------------------

cond_paste_combine_length <- function() {
  new_condition("length of ... must be at least 2", "paste_combine_length")
}
