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
  if(is.null(dt_pattern)) dt_pattern <- "^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}\\s[[:digit:]]{6}$"
  if(is.null(dt_format)) dt_format <- "%Y-%m-%d %H%M%S"

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
#' @param pattern The regularly expression to be passed to to the file.  THe default searches for any .txt or .csv file that isn't temporary
#' @return The full name of the most recent file from the stated directory
#' @export

get_recent_file <- function(dir, pattern = "^[~$]{0}.*\\.csv|\\.txt$") {
  stopifnot(dir.exists(dir))
  files <- list.files(dir, full.names = T, pattern = pattern)
  files <- files[grep("^[~$]", sub(dir, "", files, fixed = T), invert = T)]
  if(length(files) == 0) {
    stop("No files found!", call. = F)
  } else if(length(files) == 1) {
    return(files)
  } else {
    times <- file.mtime(files)
    recent <- files[times == max(times)]
    if(length(recent) > 1) warning("More than one file found!", call. = F)
    return(recent)
  }
}


#' File path in user directory
#'
#' A wrapper for creating a file name under your R_USER directory.
#'
#' @inheritParams base::file.path
#'
#' @export
#'
#' @seealso [base::file.path()]
#' @examples
#' \dontrun{
#' data(iris)
#' file_path <- user_file("outputs/tests.csv")
#' write.csv(iris, file = file_path)
#' }

user_file <- function(..., fsep = .Platform$file.sep) {
  file.path(Sys.getenv("R_USER"), ..., fsep = fsep)
}
