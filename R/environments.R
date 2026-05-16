#' List all environments and objects
#'
#' Functions to list out all environments and objects
#'
#' @details [mark::environments()] is basically a printing wrapper for
#'   [base::search()]
#'
#'   [mark::ls_all()] and [mark::objects_all()] can be used retrieved all
#'   objects from all environments in the [base::search()] path, which may print
#'   out a large result into the console.
#'
#' @inheritParams base::ls
#'
#' @export
#' @return
#' - [mark::environments()]: Invisibly, a `character` vector of environment
#' names
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
#' - [mark::ls_all()], [mark::objects_all()]: A named list for each of the
#'   environments the [base::search()] path with all the objects found in that
#'   environment
# nolint next: object_name_linter.
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

make_do_ls <- function(fun) {
  fun <- substitute(fun)
  # nolint next: object_usage_linter. False positive
  expr <- substitute(
    do_ls(..fun.., pattern = pattern, all.names = all.names, envir = envir),
    list(..fun.. = fun)
  )

  eval(
    substitute(
      as.function(
        alist(
          pattern = ,
          all.names = FALSE,
          envir = parent.frame(),
          expr
        )
      )
    )
  )
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

# nolint next: object_name_linter
do_ls <- function(FUN, pattern, all.names = FALSE, envir = parent.frame()) {
  .ls <- ls(envir = envir, pattern = pattern, all.names = all.names)
  .ls[vap_lgl(.ls, function(x) FUN(get0(x, envir = envir)))]
}
