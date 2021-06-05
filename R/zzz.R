.onAttach <- function(libname, pkgname) {

}

.onDetach <- function(libpath) {
  d <- file_path(mark_dir(), "_temp_files")
  if (is_dir(d)) unlink(d, recursive = TRUE, force = TRUE)
}
