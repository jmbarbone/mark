#' Assign add
#'
#' this needs to be tested...
#'
#' @param lhs left side statement
#' @param rhs right side statement
#'
#' @export
#'
#' @examples
#' val <- 1
#' val %=+% 2
#' val # 3
#' val %=+% 2
#' val # 5
#' \dontrun{
#' val %=+% c(1, 2, 3) # fails
#' }

`%=+%` <- function(lhs, rhs) {
  stopifnot("rhs is a vector of length lhs" = length(lhs) == length(rhs))
  e <- environment()
  assign(x = as.character(substitute(lhs, e)),
         value = lhs + rhs,
         pos = -1,
         envir = e,
         inherits = TRUE)
}
