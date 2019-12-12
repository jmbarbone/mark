#' Between more
#'
#' Additional functionality and expansion of `dplyr::between`
#'
#' @param x A numeric vector of values.
#' @params `left, right` Boundary values.
#' @param type Abrreviated type (g: greater than, ge: greater than or equal to, l: less than, le: less than or equal to)
#' @export
#' @seealso \link[dplyr][case_when]
#' @examples
#' between_more(10, 2, 10, "gl")
#' between_more(10, 2, 10, "gle")

between_more <- function(x, left, right, type = "gele") {
  if(!type %in% c("gele", "gel", "gle", "gl")) stop("Not a valid type!", call. = F)
  if(left > right) warning("`left` > `right`", call. = F)

  vapply(x, function(x) {
    switch(type,
           gele = x >= left & x <= right,
           gel  = x >= left & x < right,
           gle  = x > left & x <= right,
           gl   = x > left & x < right)
  }, logical(1))
}
