#' Between more
#'
#' Additional functionality and expansion of `dplyr::between`
#'
#' @param x A numeric vector of values
#' @param left,right Boundary values
#' @param type Abbreviation for the evaluation of `left` on `right` (see
#'   details)
#'
#' @details Type can be one of the below:
#'
#' \describe{
#'  \item{g}{is greater than (>)}
#'  \item{ge}{greater than or equal to (>=)}
#'  \item{l}{less than (<)}
#'  \item{ls}{less than or equal to (<=)}
#' }
#'
#' @returns A logical vector
#'
#' @export
#' @seealso `dplyr::case_when()`
#' @examples
#' between_more(10, 2, 10, "gl")
#' between_more(10, 2, 10, "gle")
#' between_more(1:5, c(3, 3, 2, 2, 1), 5)

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


# conditions --------------------------------------------------------------

cond_between_more_lr <- function() {
  new_condition(
    "`left` > `right`",
    "between_more_lr",
    type = "warning"
  )
}
