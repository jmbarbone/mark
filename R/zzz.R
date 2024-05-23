.onAttach <- function(libname, pkgname) {
  options(op.mark[!names(op.mark) %in% names(options())])
}

.onDetach <- function(libpath) {
  invisible()
}
