#' List all environments and objects
#'
#' Functions to list out all environments and objects
#'
#' @details
#' `environments()` is basically a printing wrapper for `base::search()`
#'
#' `ls_all()` and `objects_all()` print out way too much text
#'
#' @inheritParams base::ls
#'
#' @export
#' @name list_environments
environments <- function() {
  s <- search()

  for (e in s) {
    cat(utils::str(parent.env(as.environment(e)), give.attr = FALSE))
  }

  invisible(s)
}



#' @export
#' @rdname list_environments
ls_all <- function(all.names = FALSE) {
  sapply(search(), ls_switch, all.names = all.names)
}

#'@export
#'@rdname list_environments
objects_all <- function(all.names = FALSE) {
  ls_all(all.names = all.names)
}

ls_switch <- function(x, all.names = FALSE) {
  switch(
    x,
    R_GlobalEnv = ls(globalenv(), all.names = all.names),
    R_EmptyEnv = "",
    base = ls(baseenv(), all.names = all.names),
    ls(as.environment(x), all.names = all.names)
  )
}
