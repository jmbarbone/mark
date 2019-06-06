#' Odds ratio
#'
#' Returns the odds ratio without returning infinity
#'
#' @param .x A vector of values to repeat
#' @param `a, b, c, d` Values in the contingency table.
#' @return The odds ratio
#' @export

odds_ratio <- function(a, b, c, d, type = "hits_misses") {
  ## does not return any Inf values
  if(a < 0 || b < 0 || c < 0 || d < 0) stop("Cells cannot have negative numbers!", call. = F)
  if(type == "hits_evals") {
    ## save over for single line at end
    c <- c - a
    d <- d - b
  }
  if((b == 0) || (c == 0)) return(NA)
  if(a < 5 || b < 5 || c < 5 || d < 5) warning("Cells should all have at least 5 observations.", call. = F)
  (a * d) / (b * c)
}
