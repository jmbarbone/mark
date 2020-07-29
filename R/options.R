#' Set options
#'
#' Dynamically set options
#'
#' @param name The name of the option
#' @param value The value of the option
#'
#' @export
#'
#' @examples
#' \dontrun{
#' opt_name <- "testing.option"
#' opt_value <- TRUE
#' setOption(opt_name, opt_value)
#' getOption(opt_name)
#' opt_value <- "Jordan"
#' setOption(opt_name, opt_value)
#' getOption(opt_name)
#' opt_value <- 10
#' setOption(opt_name, opt_value)
#' getOption(opt_name)
#' }

setOption <- function(name, value) {
  # do.call(options, as.list(new_options))
  ept(sprintf('options(%s = %s)', name, optsVal(value)))
}

optsVal <- function(value) {
  UseMethod("optsVal", value)
}

optsVal.default <- function(value) {
  value
}

optsVal.character <- function(value) {
  sprintf("'%s'", value)
}
