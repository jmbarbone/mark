#' Data frame transpose
#'
#' Transposes a data.frame
#'
#' @param .data.frame A data.frame
#' @param .id The identification column number.
#' @export

t_df <- function(.data.frame, .id = 1)
{
  temp <- data.frame(t(.data.frame[-.id]), stringsAsFactors = F, row.names = NULL)
  colnames(temp) <- unlist(.data.frame[,.id], use.names = F)
  temp$.id <- colnames(.data.frame)[-.id]
  n <- ncol(temp)
  temp[c(".id", colnames(temp)[-n])]
}
