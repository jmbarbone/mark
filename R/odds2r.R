odds2r <- function(odds, n1 = NULL, n2 = n1, var = F)
{
  cohensd2r(odds2d(odds, var = var), n1 = n1, n2 = n2, var = F)
}
