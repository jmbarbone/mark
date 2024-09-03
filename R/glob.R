#' Wildcard globbing
#'
#' Helper function for globbing character vectors
#'
#' @param x A vector of characters
#' @param pattern Wildcard globbing pattern
#' @param value,... Additional parameters passed to `grep`. Note: `value` is by
#'   default `TRUE`; when `NA`, `...` is passed to `grepl`.
#' @examples
#' x <- c("apple", "banana", "peach", "pear", "orange")
#' glob(x, "*e")
#' glob(x, "pea*", value = FALSE)
#' glob(x, "*an*", value = NA)
#'
#' path <- system.file("R", package = "mark")
#' glob(list.files(path), "r*")
#' @export
glob <- function(x, pattern = NULL, value = TRUE, ...) {
  pattern <- utils::glob2rx(pattern)
  params <- rlang::list2(...)
  params$x <- x
  params$pattern <- pattern

  if (isNA(value)) {
    do.call(grepl, params)
  } else {
    params$value <- value
    do.call(grep, params)
  }
}
