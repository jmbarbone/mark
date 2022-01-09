
#' @export
`[.pseudo_id` <- function(x, i, ...) {
  new_pseudo_id(NextMethod())
}

new_pseudo_id <- function(x = integer()) {
  pseudo_id(x)
}

#' @export
vec_ptype2.pseudo_id.pseudo_id <- function(x, y, ...) {
  x
}

#' @export
vec_cast.pseudo_id.pseudo_id <- function(x, to, ...) {
  # attr(x, "uniques")[x]
  new_pseudo_id()
}


#' @export
vec_ptype_abbr.pseudo_id <- function(x, ...) {
  "psid"
}

#' @export
c.pseudo_id <- function(...) {
  x <- ..1
  y <- ..2

  # ignore recursive and use.names

  ux <- attr(x, "uniques")
  uy <- attr(y, "uniques")

  stopifnot(identical(class(ux), class(uy)))

  u <- if (is.numeric(ux)) {
    sort.int(unique(c(ux, uy)), na.last = TRUE)
  } else {
    na_last(unique(c(ux, uy)))
  }

  make_pseudo_id(c(match(ux, u)[x], match(uy, u)[y]), u)
}
