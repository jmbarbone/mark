# Smaller functions that are used internally

deparser <- function(x, env = parent.frame()) {
  if(class(substitute(x, env)) == "name") deparse(substitute(x, env)) else x
}

