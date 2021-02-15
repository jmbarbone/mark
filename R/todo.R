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
  finds <- withCallingHandlers(
    system2("git", 'grep -in "[#] TODO"', stdout = TRUE, stderr = TRUE),
    warning = function(e) {
      if (grepl("had status 1$", e$message)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  # Don't count TODO in NAMESPACE
  finds <- grep("^NAMESPACE", finds, value = TRUE, invert = TRUE)

  if (!is.null(pattern)) {
    finds <- grep(pattern = pattern, x = finds, ..., value = TRUE)
  }

  if (length(finds) == 0) {
    message("No TODOs found")
    return(invisible())
  }

  splits <- strsplit(finds, ":")
  out <- Reduce(rbind, lapply(splits, clean_todo_split))
  class(out) <- c("todos_df", "data.frame")
  out
}

clean_todo_split <- function(x) {
  n <- length(x)
  if (n < 3) {
    stop("x must have at least 3 elements", call. = FALSE)
  }

  if (n > 3) {
    x[3] <- collapse0(x[3:n], sep = ":")
    x <- x[1:3]
  }

  names(x) <- c("file", "line", "todo")
  x <- as.list(x[c(2, 1, 3)])
  x["todo"] <- sub(".*[#]\\s{0,}TODO[:]?\\s", "", x["todo"])
  x["line"] <- as.integer(x["line"])

  quick_df(x)
}

#' @exportS3Method
print.todos_df <- function(x, ...) {
  # TODO Add a limit for number of TODOs to show?
  mat <- as.matrix(x)
  n <- max(nchar(mat[, 1])) + 3L

  cat(
    sprintf("Found %d TODOS :\n\n", nrow(x)),
    apply(mat, 1, make_todo_line, n = n),
    sep = ""
  )

  invisible(x)
}

make_todo_line <- function(x, n) {
  header <- sprintf("[%s] %s", x["line"], x["file"])
  text <- str_slice_by_word(x["todo"], getOption("width") - 4L)
  pad <- collapse0(rep(" ", n))

  sprintf(
    "%s\n%s\n",
    crayon::blue(header),
    collapse0(paste0(pad, text), sep = "\n")
  )
}
