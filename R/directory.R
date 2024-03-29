#' Get recent directory
#'
#' Finds the recent subdirectory in a directory.
#'
#' @param x The root directory
#' @param ... Additional arguments passed to [mark::list_dirs()]
#' @return The full path of the most recent directory
#' @export

get_recent_dir <- function(x = ".",  ...) {
  stopifnot(dir.exists(x))
  dirs <- list_dirs(x, ...)
  newest_dir(dirs)
}

#' Get recent directory by date
#'
#' Looks at the directories and assumes the date
#'
#' @param x A directory
#' @param dt_pattern A pattern to be passed to filter for the directory
#' @param dt_format One or more formats to try
#' @param all Logical, if `TRUE` will recursively search for directories
#' @return A full path to a directory
#' @export

get_dir_recent_date <- function(
    x = ".",
    dt_pattern = NULL,
    dt_format = NULL,
    all = FALSE
) {
  dt_pattern <- dt_pattern %||% .default_dt_pattern
  dt_format <- dt_format %||% .default_dt_format
  dirs <- list_dirs(x, pattern = dt_pattern, basename = TRUE, all = all)
  ind <- sapply(
    basename(dirs),
    as.POSIXct,
    tryFormats = dt_format,
    optional = TRUE
  )
  dirs[which.max(ind)]
}

.default_dt_pattern <- paste0(
  "^[[:digit:]]{4}.?[[:digit:]]{2}.?[[:digit:]]{2}.?[[:digit:]]{2}",
  ".?[[:digit:]]{2}.?[[:digit:]]{2}(.?[PA]M)?$"
)

.default_dt_format <- c(
  "%Y-%m-%d %H %M %S",
  "%Y %m %d %H %M %S",
  "%Y-%m-%d %H%M%S",
  "%Y %m %d %H%M%S",
  "%Y%m%d %H %M %S",
  "%Y%m%d %H%M%S"
  )

#' Get recent directory by number name
#'
#' Finds the directory where the number is the greatest.  This can be useful for
#' when folders are created as run IDs.
#'
#' @param x The directory to look in
#' @return A full path to a directory
#' @export

get_dir_max_number <- function(x) {
  files <- list_dirs(x, pattern = "^[[:digit:]]+$", basename = TRUE)
  dir_int <- which.max(as.numeric(basename(files)))
  files[dir_int]
}

#' Get recent file
#'
#' A function where you can detect the most recent file from a directory.
#'
#' @param x The directory in which to search the file
#' @param exclude_temp Logical, if `TRUE` tries to remove temp Windows files
#' @param ... Additional arguments passed to [mark::list_files()]
#' @return The full name of the most recent file from the stated directory
#'
#' @export

get_recent_file <- function(x, exclude_temp = TRUE, ...) {
  stopifnot(is_dir(x))

  files <- list_files(x, ...)

  if (exclude_temp) {
    files <- remove_temp_files(files)
  }

  if (no_length(files)) {
    stop(cond_get_recent_file_none())
  }

  newest_file(files)
}

remove_temp_files <- function(x) {
  x[grep("^\\~\\$|\\~$", basename(x), invert = TRUE)]
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
#' @return A vector of full file paths
#'
#' @export

norm_path <- function(x = ".", check = FALSE, remove = check) {
  stopifnot(is.character(x))

  x <- fs::path_abs(x)
  ind <- !fs::file_exists(x)

  if (check && any(ind)) {
    warning(cond_norm_path_found(x[ind]))
  }

  if (remove) {
    x[ind] <- NA_character_
  }

  x
}

#' @export
#' @rdname norm_path
file_path <- function(..., check = FALSE, remove = check) {
  norm_path(fs::path(...), check = check, remove = remove)
}

#' @export
#' @rdname norm_path
user_file <- function(..., check = FALSE, remove = check) {
  r_user <- norm_path(Sys.getenv("R_USER"), check = TRUE)
  file_path(r_user, ..., check = check, remove = remove)
}

#' File information utils
#'
#' Other utility functions for dealing with files
#'
#' @param x A vector of file paths
#' @return A full file path
#' @export
#' @name file_info
newest_file <- function(x) {
  x <- norm_path(x, check = TRUE)
  x <- x[is_file(x)]
  x[which.max(file.mtime(x))]
}

#' @export
#' @rdname file_info
newest_dir <- function(x) {
  x <- norm_path(x, check = TRUE)
  x <- x[is_dir(x)]
  x[which.max(file.mtime(x))]
}

#' @export
#' @rdname file_info
oldest_file <- function(x) {
  x <- norm_path(x, check = TRUE)
  x <- x[is_file(x)]
  x[which.min(file.mtime(x))]
}

#' @export
#' @rdname file_info
oldest_dir <- function(x) {
  x <- norm_path(x, check = TRUE)
  x <- x[is_dir(x)]
  x[which.min(file.mtime(x))]
}

#' @export
#' @rdname file_info
largest_file <- function(x) {
  x <- norm_path(x, check = TRUE)
  x <- x[is_file(x)]
  x[which.max(file.size(x))]
}

#' @export
#' @rdname file_info
smallest_file <- function(x) {
  x <- norm_path(x, check = TRUE)
  x <- x[is_file(x)]
  x[which.min(file.size(x))]
}

#' Open a file using windows file associations
#'
#' Opens the given files(s)
#'
#' @details `open_file` is an alternative to `shell.exec()` that can take take
#' multiple files. `list_files` and `list_dirs` are mostly wrappers for
#' [fs::dir_ls()] with preferred defaults and pattern searching on the full file
#' path.
#'
#' `file_open` is simply an alias.
#'
#' @inheritParams norm_path
#' @inheritParams fs::dir_ls
#' @param pattern,glob Pattern to search for files.  `glob` is absorbed into
#'   `pattern`, through [utils::glob2rx()].
#' @param ignore_case logical. Should pattern-matching be case-insensitive?
#' @param all a logical value. If FALSE, only the names of visible files are
#'   returned (following Unix-style visibility, that is files whose name does
#'   not start with a dot). If TRUE, all file names will be returned.
#' @param basename If `TRUE` only searches pattern on the basename, otherwise on
#'   the entire path
#' @param negate Logical, if `TRUE` will inversely select files that do not
#'   match the provided pattern
#'
#' @return
#' * `open_file()`, `shell_exec()`: A logical vector where `TRUE` successfully
#' opened, `FALSE` did not and `NA` did not try to open (file not found)
#' * `list_files()`, `list_dirs()`: A vector of full paths
#' @name file_utils
NULL

#' @export
#' @rdname file_utils
open_file <- function(x) {
  x <- norm_path(x, check = TRUE)
  out <- rep(NA, length(x))
  out[!is.na(x)] <- shell_exec(x[!is.na(x)])
  out
}

#' @export
#' @rdname file_utils
file_open <- open_file

#' @rdname file_utils
#' @export
shell_exec <- function(x) {
  if (is_windows()) {
    open_fun <- function(path) shell.exec(file = path) # nolint: object_usage_linter, line_length_linter.
  } else {
    require_namespace("xopen")
    open_fun <- function(path) xopen::xopen(target = path)
  }

  x <- norm_path(x, check = TRUE)

  do_open_fun <- function(file) {
    inherits(try(open_fun(x), silent = TRUE), "try-error")
  }

  invisible(!vap_lgl(x, do_open_fun))
}

#' @rdname file_utils
#' @export
list_files <- function(
  x = ".",
  pattern = utils::glob2rx(glob),
  glob = NULL,
  ignore_case = FALSE,
  all = FALSE,
  negate = FALSE,
  basename = FALSE
) {

  pattern <- force(pattern) %|||% NULL
  path <- norm_path(x, check = TRUE)

  if (length(path) == 1L && is.na(path)) {
    return(NA_character_)
  }

  files <-
    if (basename) {
      # default behavior
      fs::dir_ls(
        path        = path,
        regexp      = pattern,
        all         = all,
        recurse     = all,
        ignore.case = ignore_case,
        invert      = negate,
        type        = "file"
      )
    } else {
      # If we want the regular expression applied to the entire file
      fs::dir_ls(
        path    = path,
        all     = all,
        recurse = all,
        type    = "file"
      )
    }

  files <- norm_path(files)
  files <- files[is_file(files)]

  if (is.null(pattern)) {
    return(files)
  }

  if (basename) {
    files[grep(
      pattern,
      basename(files),
      ignore.case = ignore_case,
      invert = negate
    )]
  } else {
    grep(
      pattern,
      files,
      ignore.case = ignore_case,
      value = TRUE,
      invert = negate
    )
  }
}

#' @rdname file_utils
#' @export
list_dirs <- function(
    x = ".",
    pattern = NULL,
    ignore_case = FALSE,
    all = FALSE,
    basename = FALSE,
    negate = FALSE
) {
  path <- norm_path(x, check = TRUE)

  if (length(path) == 1L && is.na(path)) {
    return(NA_character_)
  }

  dirs <- fs::dir_ls(path = path, type = "directory", recurse = TRUE)
  dirs <- norm_path(dirs)

  if (is.null(pattern)) {
    return(dirs)
  }

  if (basename) {
    dirs[grep(
      pattern,
      basename(dirs),
      ignore.case = ignore_case,
      invert = negate
    )]
  } else {
    grep(
      pattern,
      dirs,
      ignore.case = ignore_case,
      value = TRUE,
      invert = negate
    )
  }
}

#' Is File/Directory
#'
#' Is the path a file/directory?
#'
#' @details
#' These are essentially taken from [utils::file_test()] for `op = '-d'` and
#'   `op = -f` but separated.
#'
#' @param x A vector of file paths
#' @return A `logical` vector
#' @export

is_dir <- function(x) {
  stopifnot(!no_length(x), is.character(x))
  dir.exists(x)
}

# slightly faster than `file.exists(x) & !is_dir(x)`
#' @rdname is_dir
#' @export
is_file <- function(x) {
  stopifnot(!no_length(x), is.character(x))
  isdir <- file.info(x, extra_cols = FALSE)[["isdir"]]
  !is.na(isdir) & !isdir
}

file_create <- function(x, overwrite = FALSE) {
  dirs <- is_dir(x)
  if (any(dirs)) {
    warning(cond_file_create_dir(x[dirs]))
    x <- x[!dirs]
  }

  if (overwrite) {
    fs::file_delete(x[is_file(x)])
  }

  invisible(fs::file_create(x))
}

dir_create <- function(x, overwrite = FALSE) {
  if (overwrite) {
    e <- which(is_dir(x))
    for (i in e) {
      if (fs::dir_exists(i)) {
        fs::dir_delete(i)
      }
    }
  }

  invisible(fs::dir_create(x))
}

#' File name
#'
#' Basename of file without extension
#'
#' @inheritParams tools::file_path_sans_ext
#' @return The file name of the path without the extension
#' @export
file_name <- function(x, compression = FALSE) {
  tools::file_path_sans_ext(basename(x), compression = compression)
}

#' Add file timestamp
#'
#' Adds a timestamp to a file
#'
#' @param x A vector of files
#' @param ts A single timestamp or vector of timestamps (default: `Sys.time()`)
#' @param format A format to be applied to the times; set to `NULL` to skip
#'   formatting
#' @param sep A `character` vector of length 1 to separate the timestamp from
#'   the file name
#' @return The full name paths with the appended time stamp
#' @export
#' @examples
#' file1 <- tempfile(fileext = ".txt")
#' file2 <- tempfile()
#'
#' add_file_timestamp(file1)
#' add_file_timestamp(file2)
#'
#' file.remove(file1, file2)
add_file_timestamp <- function(
    x,
    ts = Sys.time(),
    format = "%Y-%m-%d %H%M%S",
    sep = " "
) {
  if (!is.null(format)) {
    ts <- format(ts, format = format)
  }

  bn <- file_name(x)
  ext <- fs::path_ext(x)

  if (length(sep) > 1) {
    sep <- collapse0(sep)
  }

  fs::path(dirname(x), paste0(bn, sep, ts, if (ext != "") ".", ext))
}

# conditions --------------------------------------------------------------

cond_get_recent_file_none <- function() {
  new_condition("No files found", "get_recent_file_none")
}

cond_norm_path_found <- function(paths) {
  new_condition(
    paste0("Paths not found:\n  '", collapse(paths, sep = "'\n  '"), "'"),
    "norm_path_found",
    type = "warning"
  )
}

cond_file_create_dir <- function(x) {
  new_condition(
    paste0(
      "Cannot create files that are directories:",
      paste0("\n   ", norm_path(x))
    ),
    "file_create_dir",
    type = "warning"
  )
}

# terminal line
