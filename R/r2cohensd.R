r2cohensd <- function(r) {
  if(any(r > 1 || r < -1)) stop("r values outside bounds!", call. = F)
  if(var) {
    (4 * var(r, na.rm = T)) / ((1 - r^2)^3)
  } else {
    (2 * r) / sqrt(1 - r^2)
  }
}
