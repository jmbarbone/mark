#' Get TODOs
#'
#' Search for `#`` TODO` tags
#'
#' @details
#' Calls `git grep -in "[#] TODO"` to find any lines of a `.R` or `.Rmd` file
#'   with a comment.
#'
#' @param pattern A character string containing a regular expression to filter
#'  for comments after tags; default `NULL` does not filter
#' @param path The file directory to search for the tags
#' @param ... Additional parameters passed to `grep` (Except for `pattern`, `x`,
#'   and `value`)
#' @param force If `TRUE` will force searching for files in directories that do
#'   not contain an `.Rproj` file.  This can be controlled with the option
#'   `mark.todos.force`
#'
#' @return `NULL` if none are found, otherwise a `data.frame` with the line
#'   number, file name, and TODO comment.
#'
#' @export

todos <- function(pattern = NULL, path = ".", force = getOption("mark.todos.force", FALSE), ...) {
  do_todo("todo", pattern = pattern, path = path, force = force, ...)
}

#' @rdname todos
#' @export
fixmes <- function(pattern = NULL, path = ".", force = getOption("mark.todos.force", FALSE), ...) {
  do_todo("fixme", pattern = pattern, path = path, force = force, ...)
}

do_todo <- function(text, pattern = NULL, path = path, force = FALSE, ...) {
  # fs::dir_ls() would be a lot quicker but would be a new dependency

  if (missing(path) || length(path) != 1 || !is.character(path)) {
    stop("path must be a character vector of length 1L", call. = FALSE)
  }

  if (!file.exists(path)) {
    stop("path not found: ", path, call. = FALSE)
  }

  if (length(text) != 1L) {
    stop("Length of text must be 1", call. = FALSE)
  }

  files <- if (is_dir(path)) {
    # when will path be "" ?  cusing nzchar() instead
    if (!has_char(path) | !(force | length(list.files(path, pattern = "\\.Rproj$")))) {
      message("Did not search for TODOS in ", norm_path(path))
      return(invisible(NULL))
    }

    list.files(
      path,
      pattern = "\\.r(md)?$",
      recursive = TRUE,
      ignore.case = TRUE,
      full.names = TRUE
    )
  } else {
    if (!grepl(path, pattern = "\\.r(md)?$", ignore.case = TRUE)) {
      stop("path is not a .R or .Rmd file", .call = FALSE)
    }
    path
  }

  finds <- lapply(
    lapply(files, readLines, warn = FALSE),
    function(x) {
      ind <- grep(
        pattern = sprintf("[#]\\s+%s[:]?\\s+", toupper(text)),
        x = x
      )
      quick_dfl(ind = ind, todo = x[ind])
    }
  )

  # names(finds) <- substr(files, path_n, vap_int(files, nchar))
  names(finds) <- files
  finds <- finds[vap_int(finds, nrow) > 0L]

  if (identical(remove_names(finds), list())) {
    message("No todos found")
    return(invisible(NULL))
  }

  out <- quick_df(c(
    file = list(rep(names(finds), vap_int(finds, nrow))),
    set_names0(as.list(Reduce(rbind, finds)), c("line", text))
  ))[, c("line", "file", text)]

  ind <- grepl("\\.rmd$", out[["file"]], ignore.case = TRUE)

  if (any(ind)) {
    # quick fix for Rmd files
    out[[text]][ind] <- gsub("^(<!--)\\s?|\\s?(-->)$", "", out[[text]][ind])
  }

  out[[text]] <- sub(
    sprintf("^[ #]*\\s+%s[:]?\\s+", toupper(text)),
    "",
    out[[text]]
  )

  if (!is.null(pattern)) {
    out <- out[grep(pattern, out[[text]], value = FALSE, ...), ]
    attr(out, "row.names") <- seq_along(attr(out, "row.names"))
  }

  if (nrow(out) == 0L) {
    message("No todos found")
    return(invisible(NULL))
  }

  struct(out, c("todos_df", "data.frame"), todos_type = text, .keep_attr = TRUE)
}

#' @export
print.todos_df <- function(x, ...) {
  # TODO Add a limit for number of TODOs to show?
  type <- attr(x, "todos_type")

  n <- max(nchar(x[["line"]]), 0)
  w <- getOption("width") - n - 3 # 4??
  pad <- collapse0(rep(" ", n + 3))
  pat <- sprintf("[%%%i.i]", n)

  splits <- split(x, x[["file"]])
  nm <- names(splits)

  cat0(sprintf("Found %d %s:\n", nrow(x), toupper(type)))

  for (i in seq_along(splits)) {
    catln(
      collapse0(pad, crayon_blue(nm[i])),
      apply(
        splits[[i]][, c("line", type)],
        1,
        function(xi) {
          paste(
            crayon_blue(sprintf(pat, as.integer(xi[1]))),
            if (nchar(xi[2]) > w) {
              # TODO consider wrapping with respect to the line number?
              collapse0(substr(xi[2], 1, max(1, w - 6)), " [...]")
            } else {
              xi[2]
            }
          )
        }
      )
    )
  }

  invisible(x)
}
