#' Get all from namespace
#'
#' Loads all objects from a namespace
#'
#' @description
#' Attaches package as "pseudo:`namespace`".
#'
#' @param namespace The name of the namespace
#' @inheritParams base::attach
#' @noRd

loadAllNamespace <- function(namespace, warn.conflicts = TRUE) {
  ls <- ls(all.names = TRUE, envir = asNamespace(namespace))
  ne <- new.env()

  for (i in ls) {
    assign(i, namespace %colons% i, envir = ne)
  }

  .attach <- "base" %colons% "attach"
  name <- paste0("package:", namespace)
  .attach(ne, name = name, warn.conflicts = warn.conflicts)

  invisible(NULL)
}
