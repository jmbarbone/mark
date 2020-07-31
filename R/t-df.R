#' Data frame transpose
#'
#' Transposes a data.frame
#'
#' @param x A data.frame
#' @param id The identification column number
#' @param name The name of the identification column
#' @export

t_df <- function(x, id = 1L, name = ".id") {
  temp <- data.frame(t(x[-id]), stringsAsFactors = FALSE, row.names = NULL)
  colnames(temp) <- x[[id]]
  temp$`.id` <- colnames(x)[-id]
  temp[c(name, paste(colnames(temp)[-ncol(temp)]))]
}
