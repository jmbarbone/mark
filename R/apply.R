# vaps --------------------------------------------------------------------

#' Vaps!
#'
#' Wrappers for vapply
#'
#' @details
#' These are simply wrappers for [vapply()] to shorten lines.
#'
#' @param .x A vector of values
#' @param .f A function to apply to each element in vector `.x`
#' @param .nm Logical, if `TRUE` returns names of `.x` (Note: If `.x` does not
#'   have any names, they will be set to the values)
#' @param ... Additional arguments passed to `.f`
#' @export
#' @name vap
vap_int <- function(.x, .f, ..., .nm = FALSE) {
  do_vap(.x = .x, .f = .f, .value = NA_integer_, ..., .nm = .nm)
}

#' @rdname vap
#' @export
vap_dbl <- function(.x, .f, ..., .nm = FALSE) {
  do_vap(.x = .x, .f = .f, .value = NA_real_, ..., .nm = .nm)
}

#' @rdname vap
#' @export
vap_chr <- function(.x, .f, ..., .nm = FALSE) {
  do_vap(.x = .x, .f = .f, .value = NA_character_, ..., .nm = .nm)
}

#' @rdname vap
#' @export
vap_lgl <- function(.x, .f, ..., .nm = FALSE) {
  do_vap(.x = .x, .f = .f, .value = NA, ..., .nm = .nm)
}

#' @rdname vap
#' @export
vap_cplx <- function(.x, .f, ..., .nm = FALSE) {
  do_vap(.x = .x, .f = .f, .value = NA_complex_, ..., .nm = .nm)
}

#' @rdname vap
#' @export
vap_date <- function(.x, .f, ..., .nm = FALSE) {
  out <- vap_dbl(.x, .f, ..., .nm = .nm)
  as.Date.numeric(out, origin = "1970-01-01")
}

# Mostly sets up the names
do_vap <- function(.x, .f, .value, ..., .nm) {
  if (.nm) {
    .x <- set_names0(.x, names(.x) %||% .x)
  }

  vapply(X = .x, FUN = .f, FUN.VALUE = .value, ..., USE.NAMES = .nm)
}


# This can take multiple elements, so can be a little dangerous
capply <- function(.x, .f, ..., .nm = FALSE) {
  res <- do.call("c", lapply(X = .x, FUN = .f, ...))

  use_names <- if (.nm) {
    names(.x) %||% .x
  }

  set_names0(res, use_names)
}
