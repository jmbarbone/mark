#' Insert
#'
#' Insert values at a position
#'
#' @param x A vector of values
#' @param positions Integer of positions of `x` to insert `values`
#' @param values A vector of values to insert into `x`
#'
#' @return A vector with the intended values inserted
#'
#' @examples
#' insert(letters[1:5], c(2, 4), c("X", "Y"))
#'
#' @export
#'
insert <- function(x, positions, values) {
  stopifnot(!anyNA(positions))
  positions <- as.integer(positions)

  lv <- length(values)
  lp <- length(positions)

  if (lp == 0L) {
    stop("positions has no length")
  }

  if (lv == 1L) {
    values <- rep.int(values, lp)
  } else if (lv == lp) {
    o <- order(positions)
    positions <- positions[o]
    values <- values[o]
  } else {
    stop("length(values) must be equal to length(positions) or 1")
  }

  for (i in seq_along(positions)) {
    x <- append(x, values[i], positions[i] + i - 2)
  }

  x
}
