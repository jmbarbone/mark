#' Import
#'
#' Import a single function from a package
#'
#' @param pkg String, name of the package
#' @param fun String, fun name of the function
#' @param overwrite Logical, if TRUE and `fun` is also found in the current
#'   environment, will overwrite assignment
#'
#' @export
#'
#' @examples
#' \dontrun{
#' import("magrittr", "add")
#' }

import <- function(pkg, fun, overwrite = FALSE) {
  require_namespace(pkg)
  if (fun %in% ls()) {
    stop("<<", fun, ">> has already been assigned.",
         " Use `overwite = TRUE` to overwrite assignment.",
         call. = FALSE)
  }
  assign(fun,
         eval(parse(text = sprintf("%s::%s", pkg, fun))),
         envir = parent.frame())
}
