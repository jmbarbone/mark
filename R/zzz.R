.onAttach <- function(libname, pkgname) {
  create_na_env()
}

.onDetach <- function(libpath) {
  tryCatch(remove(.na_env), warning = function(e) invisible())
  unloadNamespace("jordanExtra")
}
