#' String Close enough
#'
#' Find a value that is, eh, close enough by listed all possible values.
#' It's not the most precise but can be used to determine additional explortation.
#'
#' @param string Input vector. Either a character vector, or something coercible to one.
#' @param pattern A character string of the actual value (see below)
#' @param negate If TRUE, return non-matching elements.
#' @param ... Additional arguments passed to `stringr::regex()`
#'
#' @details pattern will be coerced like so: "abc" turned into "[a|b|c][a|b|c][a|b|c]"
#'
#' @importFrom stringr regex
#' @importFrom stringr str_detect
#' @export

str_close_enough <- function(string, pattern, ..., negate = FALSE) {
  temp <- unlist(strsplit(pattern, ""))
  unit <- paste(temp, collapse = "|")
  find <- paste0("[", paste(rep(unit, length(temp)), collapse = "]["), "]")
  stringr::str_detect(string, stringr::regex(find, ignore_case = T, ...), negate = negate)
}
