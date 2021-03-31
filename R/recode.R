#' Recode by
#'
#' A simple implementation of recoding
#'
#' @details
#' This can be comparable to [dplyr::recode()] expect that the values are
#'   arranged as `new = old` rather than `old = new` and allows for a separate
#'   vector to be passed for `new`.
#'
#' @param x A vector to recode
#' @param by A names vector (`new = old`); any non-matching values are set to
#'   the appropriate `NA`
#' @param vals An optional vector of values to use in lieu of a names in the
#'   vector; this takes priority over `names(by)`
#' @param mode passed to `as.vector()`
#'
#' @examples
#' recode_by(1:3, c(a = 1, b = 2))
#' recode_by(letters[1:3], c(`1` = "a", `2` = "b"))                   # will not guess mode
#' recode_by(letters[1:3], c(`1` = "a", `2` = "b"), mode = "integer") # make as integer
#' recode_by(letters[1:3], c("a", "b"), vals = 1:2)                   # or pass to vals
#'
#' @seealso [dplyr::recode()]
#' @export

recode_by <- function(x, by, vals = NULL, mode = "any") {
  as.vector((vals %||% names(by))[match(x, by)], mode = mode)
}
