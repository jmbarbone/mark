#' Round to
#'
#' Rounds a vector to the nearest in a set of anchors.
#'
#' @param x A vector of values
#' @param anchors A vector of anchor values
#' @export
#' @examples
#' x <- rpois(10, 1)
#' anchors <- c(0, 0.5, 1.5, 3)
#' data.frame(x, anchor = round_to(x, anchors))
round_to <- function(x, anchors) {
  x <- as.double(x)
  anchors <- as.double(anchors)
  mat <- do.call(rbind, lapply(x, function(i) i - anchors))
  mat[] <- abs(mat)
  anchors[apply(mat, 1, which.min)]
}
