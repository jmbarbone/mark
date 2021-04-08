#' Get TODOs
#'
#' Search for `#`` TODO` tags
#'
#' @details
#' Calls `git grep -in "[#] TODO"` to find any lines with a comment.
#' Removes any finds in the NAMESPACE
#'
#' @param pattern A character string containing a regular expression to filter
#'  for comments after tags; default `NULL` does not filter
#' @param ... Additional parameters passed to `grep` (Except for `pattern`, `x`,
#'   and `value`)
#'
#' @return `NULL` if none are found, otherwise a data.frame with the line
#'   number, file name, and TODO comment.
#'
#' @export

todos <- function(pattern = NULL, ...) {
  do_todo("todo", pattern = pattern, ...)
}

#' @rdname todos
#' @export
fixmes <- function(pattern = NULL, ...) {
  do_todo("fixme", pattern = pattern, ...)
}

do_todo <- function(text, pattern = NULL, ...) {
  # fs::dir_ls() would be a lot quicker but would be a new dependency
  if (length(text) != 1L) {
    stop("Length of text must be 1", call. = FALSE)
  }

  files <- list.files(pattern = "\\.[Rr]$", recursive = TRUE)
  file_list <- lapply(files, readLines, warn = FALSE)
  finds <- lapply(
    file_list,
    function(x)  {
      ind <- grep(
        pattern = sprintf("[#]\\s+%s[:]?\\s+", toupper(text)),
        x = x
      )
      quick_df(list(ind = ind, todo = x[ind]))
    }
  )
  names(finds) <- files
  ind <- vap_lgl(finds, function(x) nrow(x) > 0)
  finds <- finds[ind]

  if (!is.null(pattern)) {
    finds <- finds[grep(pattern, finds, value = FALSE, ...)]
  }

  if (identical(rename_names(finds), list())) {
    message("No todos found")
    return(invisible(NULL))
  }

  out <- cbind(rep(names(finds), vap_int(finds, nrow)), Reduce(rbind, finds))
  names(out) <- c("file", "line", text)
  out <- out[, c("line", "file", text)]
  out[[text]] <- sub(
    sprintf("^\\s{0,}[#]\\s+%s[:]?\\s+", toupper(text)),
    "",
    out[[text]]
  )
  class(out) <- c("todos_df", "data.frame")
  attr(out, "todos_type") <- text
  out
}

#' @exportS3Method
print.todos_df <- function(x, ...) {
  # TODO Add a limit for number of TODOs to show?
  type <- attr(x, "todos_type")

  n <- max(nchar(x[["line"]]))
  w <- getOption("width") - n - 3 # 4??
  pad <- collapse0(rep(" ", n + 3))
  pat <- sprintf("[%%%i.i]", n)

  splits <- split(x, x[["file"]])
  nm <- names(splits)

  cat0(sprintf("Found %d %s:\n", nrow(x), toupper(type)))

  for (i in seq_along(splits)) {
    catln(
      collapse0(pad, crayon::blue(nm[i])),
      apply(
        splits[[i]][, c("line", type)],
        1,
        function(xi) {
          paste(
            crayon::blue(sprintf(pat, as.integer(xi[1]))),
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
