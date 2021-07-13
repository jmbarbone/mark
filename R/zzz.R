.onAttach <- function(libname, pkgname) {

}

.onDetach <- function(libpath) {
  # clear out directory
  unlink(mark_dir(), recursive = TRUE)
}
