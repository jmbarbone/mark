#' Normalize a path
#'
#' Normalize and check a vector of paths
#'
#' @param x A vector of paths
#' @param check Logical, if TRUE will check if the path exists and output a
#'   warning if it does not.
#' @param remove Logical, if TRUE will remove paths that are not found
#' @param ... Character vectors for creating a path
#'
#' @export

path_norm <- function(x = ".", check = FALSE, remove = check) {
  paths <- normalizePath(x, winslash = .Platform$file.sep, mustWork = FALSE)
  ind <- !file.exists(paths)

  if (check && any(ind)) {
    warning("Paths not found:\n  ",
            paste(paths[ind], collapse = "\n  "),
            call. = FALSE)
  }

  if (remove) {
    paths[ind] <- NA_character_
  }

  paths
}

#' @export
#' @rdname path_norm
file_path <- function(..., check = FALSE, remove = check) {
  fp <- file.path(..., fsep = .Platform$file.sep)
  path_norm(fp, check = check, remove = remove)
}
