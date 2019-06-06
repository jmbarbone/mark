#' Tukey's coefficient
#'
#' Returns the coeficient for Tukey's boxplot
#'
#' @param x A vector of values
#' @return The coefficient of the distance from Q1 or Q3 by IQR values
#' @export

tukey_coef <- function(x) {
  if(!is.numeric(x)) x <- as.numeric(x)
  q1 = quantile(x, .25, names = F, na.rm = T)
  q3 = quantile(x, .75, names = F, na.rm = T)
  iqr = q3 - q1
  dplyr::case_when(
    x < q1 ~ (q1 - x) / iqr,
    x > q3 ~ (x - q3) / iqr,
    is.na(x) ~ NaN,
    T ~ 0
  )
}
