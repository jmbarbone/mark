# this needs to be tested...

`%=+%` <- function(lhs, rhs) {
  if(length(rhs) != 1) warning("rhs is a vector of length != 1")
  assign(x = as.character(substitute(lhs)),
         value = lhs + rhs,
         pos = 1,
         envir = new.env(parent = parent.frame()),
         inherits = TRUE)
}

val <- 1
val %=+% 2
val
val %=+% 2
val
val %=+% c(1, 2, 3)
val