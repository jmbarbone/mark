#' Get TODOs
#'
#' Search for `#`` TODO` tags
#'
#' @details Searches for `TODO` comments in files.  Extensions with `md`, `Rmd`,
#'   and `qmd` specifically search for a `<-- TODO * -->` string, whereas
#'   everything else is found with `# TODO`.
#'
#' @param pattern A character string containing a regular expression to filter
#'   for comments after tags; default `NULL` does not filter
#' @param path Where to search for the todos.  If this is a directory, paths
#'   matching the `ext` will be included.  If a file, `ext` is ignored.
#' @param force If `TRUE` will force searching for files in directories that do
#'   not contain an `.Rproj` file.  This can be controlled with the option
#'   `mark.todos.force`
#' @param ext A vector of file extensions to search for todos.  Ignored when
#'   `path` is not a directory or when `NULL`.
#' @param ignore A regular expression for files to ignore.  Ignored if `path` is
#'   not a directory or when `NULL`.
#' @param ... Additional parameters passed to `grep` (Except for `pattern`, `x`,
#'   and `value`)
#'
#' @return `NULL` if none are found, otherwise a `data.frame` with the line
#'   number, file name, and TODO comment.
#'
#' @examples
#' \dontrun{
#' file <- tempfile()
#' writeLines(c(
#'   "# TODO make x longer",
#'   "x <- 1:10",
#'   "length(x)",
#'   "# TODO add another example",
#'   "# FIXME This is a fixme"
#'   ), file)
#' todos(path = file)
#' todos("example", path = file)
#' fixmes(path = file)
#' file.remove(file)
#' }
#' @name todos
NULL

#' @rdname todos
#' @export
todos <- function(
    pattern = NULL,
    path = ".",
    force = getOption("mark.todos.force"),
    ext = getOption("mark.todos.ext"),
    ignore = NULL,
    ...
) {
  do_todo(
    "todo",
    pattern = pattern,
    path = path,
    force = force,
    ext = ext,
    ignore = ignore,
    ...
  )
}

#' @rdname todos
#' @export
fixmes <- function(
    pattern = NULL,
    path = ".",
    force = getOption("mark.todos.force"),
    ext = getOption("mark.todos.ext"),
    ignore = NULL,
    ...
) {
  do_todo(
    "fixme",
    pattern = pattern,
    path = path,
    force = force,
    ext = ext,
    ignore = ignore,
    ...
  )
}

do_todo <- function( # nolint: cyclocomp_linter.
    text = c("todo", "fixme"),
    pattern = NULL,
    path = ".",
    force = getOption("mark.todos.force"),
    ext = getOption("mark.todos.ext"),
    ignore = NULL,
    ...
) {
  text <- match_param(text)
  if (
    missing(path) ||
    length(path) != 1 ||
    !is.character(path)
  ) {
    stop(cond_do_todo_path())
  }

  stopifnot(fs::file_exists(path))

  ls <- rlang::list2(...)

  if (is_dir(path)) {
    rproj_found <- local({
      extensions <- tools::file_ext(fs::dir_ls(path, type = "file"))
      any(tolower(extensions) == "rproj")
    })

    if (!(rproj_found || force)) {
      message("Did not search for TODOS in ", norm_path(path))
      return(invisible(NULL))
    }

    files <- fs::dir_ls(
      path,
      recurse = TRUE,
      ignore.case = TRUE,
      type = "file"
    )

    if (!is.null(ext)) {
      files <- files[tolower(tools::file_ext(files)) %in% tolower(ext)]
    }

    if (!is.null(ignore)) {
      params <- ls
      params$pattern <- ignore
      params$x <- files
      params$invert <- TRUE
      params$value <- TRUE
      files <- do.call(grep, params)
    }
  } else {
    files <- path
  }

  finds <- lapply(
    lapply(files, readLines, warn = FALSE, skipNul = TRUE),
    function(x, regex) {
      x <- enc2utf8(x)
      ind <- grep(pattern = regex, x = x)
      quick_dfl(ind = ind, todo = x[ind])
    },
    regex = sprintf("[#]\\s+%s[:]?\\s+", toupper(text))
  )

  names(finds) <- files
  finds <- finds[vap_int(finds, nrow) > 0L]

  if (identical(remove_names(finds), list())) {
    message("No todos found")
    return(invisible(NULL))
  }

  out <- quick_df(c(
    file = list(rep(names(finds), vap_int(finds, nrow))),
    set_names(as.list(Reduce(rbind, finds)), c("line", text))
  ))

  out[["file"]] <- fs::path_rel(out[["file"]], getwd())
  out <- out[, c("line", "file", text)]
  ind <- tolower(tools::file_ext(out[["file"]])) %in% c("md", "qmd", "rmd")

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
    params <- ls
    params$pattern <- pattern
    params$x <- out[[text]]
    params$value <- FALSE
    out <- out[do.call(grep, params), , drop = FALSE]
    attr(out, "row.names") <- seq_along(attr(out, "row.names")) # nolint: object_name_linter, line_length_linter.
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
  catln(sprintf("Found %d %s(s):", nrow(x), toupper(type)))

  chunks <- split(as.data.frame(x), x[["file"]])
  nms <- names(chunks)
  n <- max(nchar(x$line))
  pad <- strrep("\u00a0", n + 3L)

  if (!isFALSE(getOption("mark.todos..norm_path"))) {
    # perform action on chunks as not to modify x
    for (i in seq_along(chunks))
    chunks <- lapply(chunks, function(chunk) {
      chunk[["file"]] <- norm_path(chunk[["file"]])
      chunk
    })
  }

  for (i in seq_along(nms)) {
    cli::cli_text(sprintf(
      "%s{.file %s}",
      pad,
      nms[i]
    ))

    for (j in seq_len(nrow(chunks[[i]]))) {
      cli::cli_text(sprintf(
        "{.href [%s](file://%s#%i)} %s",
        format_line_number(chunks[[i]][["line"]][j], width = n),
        chunks[[i]][["file"]][j],
        chunks[[i]][["line"]][j],
        string_dots(chunks[[i]][[3L]][j], getOption("width") - (n + 3L))
      ))
    }
  }

  return(invisible(x))
}

format_line_number <- function(x, width = 3) {
  x <- format(x, width = width)
  x <- gsub("\\s", "\u00a0", x)
  x <- sprintf("[%s]", x)
  crayon_blue(x)
}

string_dots <- function(x, width = getOption("width")) {
  long <- nchar(x) > width
  x[long] <- paste(strtrim(x[long], width - 5), "[...]")
  x
}

# conditions --------------------------------------------------------------

cond_do_todo_path <- function() {
  new_condition("path must be a character vector of length 1L", "do_todo_path")
}

cond_do_todo_path_r <- function() {
  new_condition("path is not a .R or .Rmd file", "do_todo_path_r")
}
