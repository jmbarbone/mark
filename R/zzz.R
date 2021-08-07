.onAttach <- function(libname, pkgname) {
  options(op.mark[names(op.mark) %out% names(options())])
}

.onDetach <- function(libpath) {

}
