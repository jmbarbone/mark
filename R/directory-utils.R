#' Get recent directory
#'
#' Finds the recent subdirectory in a directory.
#'
#' @param .dir The root directory
#' @param recursive From `list.dirs()`
#' @param ... Further arguments passed to `list.dirs()`
#' @return The full path of the most recent directory
#' @export

get_recent_dir <- function(.dir, recursive = FALSE, ...) {
  stopifnot(dir.exists(.dir))
  df <- Reduce(rbind, lapply(list.dirs(.dir), file.info, recursive = recursive, ...))
  rownames(df)[which.max(df$mtime)]
}


#' Get recent directory by folder name
#'
#' Finds the most recent subdirectory assuming it is a number or date
#'
# get_recent_dir_name <- function(.dir, use_format = "numeric") {
#   mat <- Reduce(rbind, lapply(list.dirs(.dir), file.info, recursive = F))
#   sapply(rownames(mat), function(x) {
#     sapply(gsub(paste0("^", pattern = dirname(x), "[/|\\]+"), x = x, replacement = ""), determine_format)
#     determine_format <- switch(use_format,
#                                "numeric" = as.numeric(x),
#                                "date" = as.Date(x, format = format),
#                                "datetime" = as.POSIXlt(x, format = format))
#   }, USE.NAMES = FALSE)
# }

#' Get recent directory by date
#'
#' Looks at the directories and assumes the date
#'
#' @param dirs A directory.
#' @param dt_pattern A pattern to be passed to filter for the directory.
#' @param dt_format A format to be passed for the date.
#' @export

get_dir_recent_date <- function(dirs, dt_pattern = NULL, dt_format = NULL) {
  if (is.null(dt_pattern)) dt_pattern <- "^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}\\s[[:digit:]]{6}$"
  if (is.null(dt_format)) dt_format <- "%Y-%m-%d %H%M%S"

  # as.POSIXlt(x, tz = "", format,
  #            tryFormats = c("%Y-%m-%d %H:%M:%OS",
  #                           "%Y/%m/%d %H:%M:%OS",
  #                           "%Y-%m-%d %H:%M",
  #                           "%Y/%m/%d %H:%M",
  #                           "%Y-%m-%d",
  #                           "%Y/%m/%d",
  #                           "%Y-%b-%d %H%M%S"))

  dir_int <- which.max(suppressWarnings(sapply(list.files(dirs, pattern = dt_pattern), as.POSIXct, dt_format, USE.NAMES = F)))
  list.files(dirs, pattern = dt_pattern, full.names = T)[dir_int]
}

#' Get recent directory by number name
#'
#' Finds the directory where the number is the greatest.  This can be useful for when folders are created as run IDs.
#'
#' @param dir The directory to look in.
#' @export

get_dir_max_number <- function(dir) {
  dir_int <- which.max(list.files(dir, pattern = "^[:digit:]+$"))
  list.files(dir, pattern = "^[:digit:]+$", full.names = T)[dir_int]
}


#' Get recent file
#'
#' A function where you can detect the most recent file from a directory.
#'
#' @param dir The directory in which to search the file
#' @param pattern The regularly expression to be passed to to the file
#' @param negate Logical, if TRUE, files with matching `pattern` will be removed
#' @param exclude_temp Logical, if TRUE files that begin with "^\\~\\$" are excluded
#' @param recursive Logical, passed to `list.files(., recursive)`
#' @return The full name of the most recent file from the stated directory
#'
#' @export

get_recent_file <- function(dir, pattern = NULL, negate = FALSE, exclude_temp = TRUE, recursive = FALSE) {
  stopifnot(dir.exists(dir))
  files <- if (negate) {
    lf <- list.files(dir, pattern = NULL, recursive = recursive)
    grep(pattern, lf, value = TRUE, invert = TRUE)
  } else {
    list.files(dir, pattern = pattern, recursive)
  }

  if (exclude_temp) {
    files <- grep("^\\~\\$", files, value = TRUE, invert = TRUE)
  }

  res <- file_path(dir, files)

  if (!length(res)) {
    stop("No files found.", call. = FALSE)
  } else if (length(res) == 1) {
  } else {
    times <- file.mtime(res)
    res <- res[times == max(times)]

    if (length(res) > 1) {
      warning("More than one file found.", call. = FALSE)
    }
  }
  res
}



#' Normalize paths
#'
#' Normalize and check a vector of paths
#'
#' @param x A character vector of paths
#' @param check Logical, if TRUE will check if the path exists and output a
#'   warning if it does not.
#' @param remove Logical, if TRUE will remove paths that are not found
#' @param ... Character vectors for creating a path
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(iris)
#' file_path <- user_file("outputs/tests.csv")
#' write.csv(iris, file = file_path)
#' }
#'

norm_path <- function(x = ".", check = FALSE, remove = check) {
  stopifnot("x (path) must be a character vector" = is.character(x))

  paths <- normalizePath(x, winslash = .Platform$file.sep, mustWork = FALSE)
  ind <- !file.exists(paths)

  if (check && any(ind)) {
    warning("Paths not found:\n  '",
            paste(paths[ind], collapse = "'\n  '"),
            "'",
            call. = FALSE)
  }

  if (remove) {
    paths[ind] <- NA_character_
  }

  paths
}

#' @export
#' @rdname norm_path
file_path <- function(..., check = FALSE, remove = check) {
  fp <- file.path(..., fsep = .Platform$file.sep)
  norm_path(fp, check = check, remove = remove)
}

#' @export
#' @rdname norm_path
user_file <- function(..., check = FALSE, remove = check) {
  file_path(Sys.getenv("R_USER"), ..., check = check, remove = remove)
}

#' File information utils
#'
#' Other utility functions for dealing with files
#'
#' @param x A vector of file paths
#' @export
#' @name file_info
newest_file <- function(x) {
  x <- norm_path(x, check = TRUE)
  x[which.max(file.mtime(x))]
}

#' @export
#' @rdname file_info
oldest_file <- function(x) {
  x <- norm_path(x, check = TRUE)
  x[which.min(file.mtime(x))]
}

#' @export
#' @rdname file_info
largest_file <- function(x) {
  x <- norm_path(x, check = TRUE)
  x[which.max(file.size(x))]
}

#' @export
#' @rdname file_info
smallest_file <- function(x) {
  x <- norm_path(x, check = TRUE)
  x[which.min(file.size(x))]
}


#' Open a file using windows file associations
#'
#' Opens the given files(s)
#'
#' @details
#' `open_file` is an alternative to [base::shell.exec()] that can take take
#'   multiple files.
#' `list_files` is mostly a wrapper for [base::list.files()] with preferred
#'   defaults.
#'
#' @inheritParams norm_path
#' @inheritParams base::list.files
#' @param ignore_case logical. Should pattern-matching be case-insensitive?
#' @param all a logical value. If FALSE, only the names of visible files are
#'   returned (following Unix-style visibility, that is files whose name does
#'   not start with a dot). If TRUE, all file names will be returned.
#'
#' @export
open_file <- function(x) {
  x <- norm_path(x, check = TRUE)
  shell_exec(x)
}

#' @rdname open_file
#' @export
shell_exec <- function(x) {
  for (i in x) {
    cm <- sprintf('start "" "%s"', shQuote(i))
    shell(cm, wait = FALSE, translate = TRUE)
  }
}

#' @rdname open_file
#' @export
list_files <- function(x = ".", pattern = NULL, ignore_case = FALSE, all = FALSE) {
  path <- norm_path(x, check = TRUE)
  if (is.na(path)) {
    return(NA_character_)
  }
  out <- list.files(
    path = x,
    pattern = pattern,
    all.files = all,
    full.names = TRUE,
    recursive = all,
    ignore.case = ignore_case,
    include.dirs = FALSE,
    no.. = !all
  )
  norm_path(out)
}
