#' Between more
#'
#' Additional functionality and expansion of `dplyr::between`
#'
#' @param x A numeric vector of values.
#' @param left,right Boundary values.
#' @param type Abbreviation for the evaluation of `left` on `right`
#'
#' @details
#' Type can be one of the below:
#' \itemize{
#'  \item{"\strong{g}": }{is greater than (>)}
#'  \item{"\strong{ge}": }{greater than or equal to (>=)}
#'  \item{"\strong{l}": }{less than (<)}
#'  \item{"\strong{le}": }{less than or equal to (<=)}
#' }
#'
#' @export
#'
#' @seealso [dplyr::case_when()]
#' @examples
#' between_more(10, 2, 10, "gl")
#' between_more(10, 2, 10, "gle")

between_more <- function(x, left, right, type = c("gele", "gel", "gle", "gl")) {
  type <- match_param(type)
  if(left > right) warning("`left` > `right`", call. = FALSE)

  switch(type,
         gele = x >= left & x <= right,
         gel  = x >= left & x < right,
         gle  = x > left & x <= right,
         gl   = x > left & x < right)
}
