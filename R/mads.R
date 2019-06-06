mads <- function(x, constant = 1.4826, ...) {
  (x - median(x)) / mad(x, constant = constant, ...)
}
