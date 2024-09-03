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

  nval <- length(values)
  npos <- length(positions)

  if (npos == 0L) {
    stop(cond_insert_npos())
  }

  if (nval == 1L && !is.list(values)) {
    values <- rep.int(values, npos)
  } else if (nval == npos) {
    o <- order(positions)
    positions <- positions[o]
    values <- values[o]
  } else {
    stop(cond_insert_length())
  }

  seqs <- seq_along(positions)
  positions <- positions + seqs - 1L

  for (i in seq_along(positions)) {
    x <- append0(x, values[i], positions[i])
  }

  x
}


# conditions --------------------------------------------------------------

cond_insert_npos <- function() {
  new_condition(
    "positions has no length",
    "insert_npos"
  )
}

cond_insert_length <- function() {
  new_condition(
    "length(values) must be equal to length(positions) or 1",
    "insert_length"
  )
}
