z_score <- function(x, na.rm = F) {
  (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
}
