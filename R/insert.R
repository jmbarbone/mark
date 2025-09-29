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
insert <- function(x, positions, values) {
  stopifnot(!anyNA(positions))
  positions <- as.integer(positions)
  stopifnot(length(positions) >= 1)

  nval <- length(values)
  npos <- length(positions)

  if (nval == 1L && !is.list(values)) {
    values <- rep.int(values, npos)
  } else if (nval == npos) {
    o <- order(positions)
    positions <- positions[o]
    values <- values[o]
  } else {
    stop(unequal_lengths())
  }

  seqs <- seq_along(positions)
  positions <- positions + seqs - 1L

  for (i in seq_along(positions)) {
    x <- append0(x, values[i], positions[i])
  }

  x
}


# conditions --------------------------------------------------------------

unequal_lengths := condition(
  "lengths of positions and values are unequal",
  type = "error",
  exports = "insert",
  help = c(
    "insert() requires that `positions` and `values` are the same length or",
    " that `values` is length 1"
  )
)
