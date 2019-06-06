sd_pooled <- function(n1, n2, se1, se2, max = F) {
  s1 <- se1 * sqrt(n1)
  s2 <- se2 * sqrt(n2)
  if(max) {a <- 0} else {a <- -2}
  sp <- sum((n1 - 1) * s1^2, (n2 - 1) * s2^2) / sum(n1, n2, a)
  sqrt(sp)
}
