#' Data frame transpose
#'
#' Transposes a data.frame
#'
#' @param x A data.frame
#' @param id The identification column number.
#' @export

t_df <- function(x, id = 1L) {
  temp <- data.frame(t(as.data.frame(x)[-id]), stringsAsFactors = FALSE, row.names = NULL)
  colnames(temp) <- paste(unlist(x[, id], use.names = FALSE))
  temp$.id <- colnames(x)[-id]
  temp[c(".id", paste(colnames(temp)[-ncol(temp)]))]
}
