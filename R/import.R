#' Import
#'
#' Import a single function from a package
#'
#' @param pkg String, name of the package
#' @param fun String, fun name of the function
#' @param overwrite Logical, if TRUE and `fun` is also found in the current
#'   environment, will overwrite assignment
#' @return None, called for side effects
#' @export
#'
#' @examples
#' # assigns `add` -- test with caution
#' import("magrittr", "add")

import <- function(pkg, fun, overwrite = FALSE) {
  e <- parent.frame()
  require_namespace(pkg)

  if (!overwrite && fun %in% ls(envir = e)) {
    stop("`", fun, "` has already been assigned.",
         " Use `overwite = TRUE` to overwrite assignment.",
         call. = FALSE)
  }

  assign(fun, pkg %colons% fun, envir = e)
}
