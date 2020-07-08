#' Deprecate a function with a message
#'
#' Adds adds a note that
#'
#' @param old_function Function to deprecate
#' @param old_package Package for old function
#' @param new_function New function to replace
#' @param new_package New package for replacing function
#'
#' @export
#' @examples
#' deprecate_fun("jordan", "fake_fun", new_function = "real_fun")
#' # Running a second time won't produce message
#' deprecate_fun("jordan", "fake_fun", new_function = "real_fun")

deprecate_fun <- function(old_package,
                          old_function,
                          new_package = old_package,
                          new_function = old_function) {
  stopifnot(!(old_function == new_function & old_package == new_package))
  opt <- sprintf("%s.%s.deprecated", old_package, old_function)
  op <- getOption(opt, default = FALSE)
  if (!op) {
    setOption(opt, TRUE)
    m <- sprintf("%s::%s is deprecated, please use %s::%s instead.",
                 old_package, old_function, new_package, new_function)
    warning(m, "\nThis message will only show once per session.", call. = FALSE)
  } else {
    invisible(TRUE)
  }
}
