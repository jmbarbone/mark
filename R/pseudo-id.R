#' Create an ID for a vector
#'
#' Transforms a vector into an integer of IDs.
#'
#' @param x A vector of values
#' @param ... Additional arguments passed to methods
#'
#' @returns A `pseudo_id` object where the `integer` value of the vector
#' correspond to the position of the unique values in the attribute `"uniques"`.
#'
#' @examples
#' set.seed(42)
#' (x <- sample(letters, 10, TRUE))
#' (pid <- pseudo_id(x))
#' attr(pid, "uniques")[pid]
#'
#' @export
pseudo_id <- function(x, ...) {
  UseMethod("pseudo_id", x)
}

#' @export
#' @rdname pseudo_id
pseudo_id.pseudo_id <- function(x, ...) {
  x
}

#' @export
#' @rdname pseudo_id
#' @param na_last `Logical` if `FALSE` will not place `NA` at the end
pseudo_id.default <- function(x, na_last = TRUE, ...) {
  ux <- unique(x)
  if (na_last) ux <- na_last(ux)
  make_pseudo_id(match(x, ux), ux)
}

#' @export
#' @rdname pseudo_id
pseudo_id.factor <- function(x, ...) {
  x <- fact(x)
  pseudo_id(fact_coerce_levels(levels(x))[x])
}


#' @export
#' @param all if `TRUE` will print all uniques.  This is not recommend for many
#'   uniques as it will crowd the console output
print.pseudo_id <- function(x, ..., all = FALSE) {
  print(as.integer(x))
  out <- collapse0("Uniques: ", paste0(attr(x, "uniques"), sep = " "), sep = "")
  if (!all) {
    width <- getOption("width", 180)
    if (nchar(out) > width) {
      out <- substr(out, 1, width - 4)
      out <- paste0(out, " ...")
    }
  }
  cat0(out, "\n")
  invisible(x)
}


# helpers -----------------------------------------------------------------

pseudo_id_factor <- function(x) {
  # This should be used for when the the levels all need to be retained
  # pseudo_id() only returns values present in x
  x <- fact(x)
  lvl <- levels(x)

  # remake to be integers
  x <- seq_along(lvl)[x]
  x[is.na(x)] <- length(lvl)

  # clean up order of new lvls
  ux <- na_last(lvl[order(match(lvl, lvl[x]))])
  make_pseudo_id(match(lvl, ux)[x], ux)
}


make_pseudo_id <- function(x, u) {
  struct(x, class = c("pseudo_id", "integer"), uniques = u)
}

na_last <- function(x) {
  if (anyNA(x)) {
    nas <- is.na(x)
    c(x[!nas], x[nas])
  } else {
    x
  }
}

.uniques <- function(x) {
  attr(x, "uniques")
}

