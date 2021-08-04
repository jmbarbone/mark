.onAttach <- function(libname, pkgname) {
  options(op.mark[names(op.mark) %out% names(options())])
  mark_dir_remove()
}

.onDetach <- function(libpath) {
  mark_dir_remove()
}
