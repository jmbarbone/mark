.onAttach <- function(libname, pkgname) {
  create_na_env()
}

.onDetach <- function(libname, pkgname) {
  remove(.na_env)
}
