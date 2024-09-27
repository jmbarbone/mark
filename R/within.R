#' within boundaries
#'
#' Compare a vector within (between) other values
#'
#' @param x A numeric vector of values
#' @param left,right Boundary values.  For [within()], when `NULL` no
#'   comparison is made for that boundary.  When both are `NULL`, `x` is just
#'   returned.
#'
#' @details `type``, `bounds`` can be one of the below:
#'
#' \describe{
#'  \item{g,(}{is greater than (>)}
#'  \item{ge,[}{greater than or equal to (>=)}
#'  \item{l,))}{less than (<)}
#'  \item{le,[]}{less than or equal to (<=)}
#' }
#'
#' Note: [between_more()] may be deprecated in the future in favor of just
#' [within()]
#'
#' @returns A logical vector
#'
#' @examples
#'
#' between_more(2:10, 2, 10, "gl")
#' within(2:10, 2, bounds = "()")
#' between_more(10, 2, 10, "gle")
#' within(2:10, bounds = "(]")
#' within(1:5, c(3, 3, 2, 2, 1), 5)
#' @name within
#' @aliases between betwee_more
NULL

# TODO consider deprecating `between_more()` in favor of `within()``

#' @rdname within
#' @export
#' @param type Abbreviation for the evaluation of `left` on `right` (see
#'   details)
between_more <- function(x, left, right, type = c("gele", "gel", "gle", "gl")) {
  type <- match_param(type)

  if (any(left > right, na.rm = TRUE)) {
    warning(cond_between_more_lr())
  }

  switch(
    type,
    gele = x >= left & x <= right,
    gel  = x >= left & x < right,
    gle  = x > left & x <= right,
    gl   = x > left & x < right
  )
}

#' @rdname within
#' @export
#' @param bounds Boundaries for comparisons of `left` and `right` (see details)
within <- function(
    x,
    left = NULL,
    right = NULL,
    bounds = c("[]", "[)", "(]", "()")
) {
  left_null <- is.null(left)
  right_null <- is.null(right)

  if (left_null && right_null) {
    return(x)
  }

  if (any(left > right, na.rm = TRUE)) {
    warning(cond_within_lr())
  }

  funs <- switch(
    match_param(bounds),
    "[]" = c(">=", "<="),
    "[)" = c(">=", "<"),
    "(]" = c(">", "<="),
    "()" = c(">", "<")
  )

  if (left_null) {
    left <- TRUE
  } else {
    left <- do.call(funs[1], list(x, left))
  }

  if (right_null) {
    right <- TRUE
  } else {
    right <- do.call(funs[2], list(x, right))
  }

  left & right
}

# conditions --------------------------------------------------------------

cond_between_more_lr <- function() {
  new_condition(
    "`left` > `right`",
    "between_more_lr",
    type = "warning"
  )
}

cond_within_lr <- function() {
  new_condition(
    "`left` > `right`",
    "within_lr",
    type = "warning"
  )
}
