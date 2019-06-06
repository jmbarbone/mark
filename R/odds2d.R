odds2d <- function(odds, var = F)
{
  if(var) {
    var(log(odds), na.rm = T) * sqrt(3) / pi^2
  }  else {
    log(odds) * sqrt(3) / pi
  }
}

