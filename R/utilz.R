# Smaller functions that are used internally

deparser <- function(x, env = parent.frame()) {
  if(class(substitute(x, env)) == "name") deparse(substitute(x, env)) else x
}


# Happily ripped from: http://r-pkgs.had.co.nz/description.html

require_namespace <- function(namespace) {
  if (!requireNamespace(namespace, quietly = TRUE)) {
    stop(sprintf("Package \"%s\" needed for this function to work. Please install it.", namespace),
         call. = FALSE)
  }
}
