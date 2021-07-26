#' List all environments and objects
#'
#' Functions to list out all environments and objects
#'
#' @details
#' `environments()` is basically a printing wrapper for `base::search()`
#'
#' `ls_all()` and `objects_all()` can be used retrieved all objects from all
#'   environments in the `search()` path, which may print out a large result
#'   into the console.
#'
#' @inheritParams base::ls
#'
#' @export
#' @return
#' * `environments()`: Invisibly, a `character` vector of environment names
#' @name list_environments
environments <- function() {
  struct(search(), c("character", "mark_environments"))
}

#' @export
#' @rdname list_environments
#' @param x A mark_environments object
#' @param ... Not currently used
print.mark_environments <- function(x, ...) {
  for (e in x) {
    cat(utils::str(parent.env(as.environment(e)), give.attr = FALSE))
  }
  invisible(x)
}

#' @export
#' @rdname list_environments
#' @return
#' * `ls_all()`, `objects_all()`: A named list for each of the environments in the `search()` path with all the objects found in that environment
ls_all <- function(all.names = FALSE) {
  sapply(search(), ls_switch, all.names = all.names)
}

#'@export
#'@rdname list_environments
objects_all <- ls_all

ls_switch <- function(x, all.names = FALSE) {
  switch(
    x,
    R_GlobalEnv = ls(globalenv(), all.names = all.names),
    R_EmptyEnv = "",
    base = ls(baseenv(), all.names = all.names),
    ls(as.environment(x), all.names = all.names)
  )
}


#' List Objects - extensions
#'
#' @inheritParams base::ls
#' @return A `character` vector of names
#' @export
#' @name ls_ext

#' @export
#' @rdname ls_ext
ls_dataframe <- function(pattern, all.names = FALSE) {
  do_ls(is.data.frame, pattern = pattern, all.names = all.names)
}

#' @export
#' @rdname ls_ext
ls_function <- function(pattern, all.names = FALSE) {
  do_ls(is.function, pattern = pattern, all.names = all.names)
}

#' @export
#' @rdname ls_ext
ls_object <- function(pattern, all.names = FALSE) {
  do_ls(is.object, pattern = pattern, all.names = all.names)
}

do_ls <- function(FUN, pattern, all.names = FALSE) {
  .ls <- ls(envir = parent.frame(3), pattern = pattern)
  .ls[vap_lgl(.ls, function(x) FUN(get0(x)))]
}
