cohensd2r <- function(d, n1 = NULL, n2 = n1, var = F) {
  if(is.null(n1)) {
    a <- 4
  } else {
    a <- sum(n1, n2)^2 / prod(n1, n2)
  }
  if(var) {
    (a^2 * var(d, na.rm = T)) / ((d^2 + a)^3)
  } else {
    d / sqrt(d^2 + a)
  }
}
