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
  sapply(search(), function(s) ls(as.environment(s), all.names = all.names))
}

#'@export
#'@rdname list_environments
objects_all <- ls_all

#' List Objects - extensions
#'
#' @inheritParams base::ls
#' @return A `character` vector of names
#' @name ls_ext
NULL

make_do_ls <- function(FUN) {
  FUN <- match.fun(FUN)
  function(pattern, all.names = FALSE, envir = parent.frame()) {
    do_ls(FUN, pattern = pattern, all.names = all.names, envir = envir)
  }
}

#' @export
#' @rdname ls_ext
ls_dataframe <- make_do_ls(is.data.frame)

#' @export
#' @rdname ls_ext
ls_function <- make_do_ls(is.function)

#' @export
#' @rdname ls_ext
ls_object <- make_do_ls(is.object)

do_ls <- function(FUN, pattern, all.names = FALSE, envir = parent.frame()) {
  .ls <- ls(envir = envir, pattern = pattern, all.names = all.names)
  .ls[vap_lgl(.ls, function(x) FUN(get0(x, envir = envir)))]
}
