#' Logic - Extension'
#'
#' Logical operations, extended
#'
#' @description
#' All functions take logical or logical-like (i.e., 1, 0, or NA as integer or
#'   doubles) and return logical values.
#'
#' Extensions to the base logical operations to account for `NA` values.
#'
#' [base::isTRUE()] and b[ase::isFALSE()] will only return single length `TRUE`
#'   or `FALSE` as it checks for valid lengths in the evaluation.  When needing
#'   to check over a vector for the presense of `TRUE` or `FALSE` and not being
#'   held back by `NA` values, `is_true` and `is_false` will always provide a
#'   `TRUE` `FALSE` when the vector is logical or return `NA` is the vector `x`
#'   is not logical.
#'
#' `%or%` is just a wrapper for [base::xor()]
#'
#' @param x,y  A vector of logical values.  If `NULL` will generate a warning.  If
#'   not a logical value, will return `NA` equal to the vector length
#' @param ... Vectors or a list of logical values
#' @param na.rm Logical, if `TRUE` will ignore `NA`
#' @export
#' @name logic_ext

is_true <- function(x) {
  null_check(x)

  out <- as.logical(x)
  if (!is_boolean(x)) {
    return(out)
  }

  out[is.na(out)] <- FALSE
  out
}

#' @export
#' @rdname logic_ext
is_false <- function(x) {
  null_check(x)

  out <- as.logical(x)
  if (!is_boolean(x)) {
    return(out)
  }

  out[is.na(out)] <- TRUE
  !out
}

#' @export
#' @rdname logic_ext
`%or%` <- function(x, y) {
  xor(x, y)
}

#' @export
#' @rdname logic_ext
OR <- function(..., na.rm = FALSE) {
  mat <- cbind(...)
  apply_logical_matrix(mat, "|", na.rm = na.rm)
}

#' @export
#' @rdname logic_ext
AND <- function(..., na.rm = FALSE) {
  mat <- cbind(...)
  apply_logical_matrix(mat, "&", na.rm = na.rm)
}


# FUNS --------------------------------------------------------------------


null_check <- function(x) {
  if (length(x) == 0L || is.null(x)) {
    stop("Cannot accept `NULL` or 0 length values",
         call. = FALSE)
  }
}

apply_logical_matrix <- function(mat, FUN, na.rm) {
  stopifnot("Not a matrix" = is.matrix(mat),
            "Not logical" = is_boolean(mat))

  use_val <- switch(FUN, `|` = FALSE, `&` = TRUE)
  use_fun <- match.fun(FUN)

  apply(
    mat,
    1,
    function(x, na.rm = FALSE) {
      if (na.rm) {
        x <- x[!is.na(x)]
      }

      len <- length(x)

      if (len == 0L) {
        if (na.rm) use_val else NA
      } else if (len == 1L) {
        x
      } else {
        Reduce(use_fun, x)
      }
    },
    na.rm = na.rm
  )
}

is_boolean <- function(x) {
  if (is.logical(x)) {
    TRUE
  } else if (is.numeric(x)) {
    bools <- x %in% c(1, 0)
    bools[is.na(x)] <- TRUE
    all(bools)
  } else {
    FALSE
  }
}
