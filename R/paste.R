#' Paste combine
#'
#' Paste and combine
#'
#' @param x,y Vectors to paste and combine
#' @param collate Logical; `TRUE` prints out combinations in order of vector
#'   `x` elements then `y`; otherwise reversed (see examples)
#' @param sep A character string to separate terms
#' @export
#'
#' @examples
#' x <- letters[1:5]
#' y <- 1:3
#' paste_c(x, y)
#' paste_c(x, y, sep = "_")
#' paste_c(x, y, collate = FALSE)

paste_c <- function(x, y, collate = TRUE, sep = "") {
  out <- outer(x, y, FUN = paste, sep = sep)
  if (collate) {
    out <- apply(out, 1, c)
  }
  as.vector(out, "character")
}
