#' Assign add
#'
#' this needs to be tested...
#'
#' @param lhs left side statement
#' @param rhs right side statement

`%=+%` <- function(lhs, rhs) {
  if(length(rhs) != 1) warning("rhs is a vector of length != 1")
  e <- parent.frame()
  assign(x = as.character(substitute(lhs)),
         value = lhs + rhs,
         pos = -1,
         envir = e,
         inherits = TRUE)
}

# val <- 1
# val %=+% 2
# val
# val %=+% 2
# val
# val %=+% c(1, 2, 3)
# val