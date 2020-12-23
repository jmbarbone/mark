.onAttach <- function(libname, pkgname) {
  create_na_env()
}

.onDetach <- function(libpath) {
  remove(.na_env)
}
