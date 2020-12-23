#' Temporary plotting
#'
#' Reset par() after running
#'
#' @param ... Code to be evaluated
#' @param ops A named list to be passed to [graphics::par()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' with_par(
#'   plot(lm(Sepal.Length ~ Sepal.Width, data = iris)),
#'   plot(lm(Petal.Length ~ Petal.Width, data = iris)),
#'   ops = list(mfrow = c(2, 4))
#' )
#' }

with_par <- function(..., ops = NULL) {
  if ("package:graphics" %out% search()) {
    warning("Pacakge `graphics` will be loaded", call. = FALSE)
    require("graphics")
  }

  par0 <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(par0), add = TRUE)
  graphics::par(ops)
  invisible(eval(...))
}
