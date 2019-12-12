#' Get recent file
#'
#' Get the recent file from a directory
#'
#' @param dir A directory.
#' @param pattern A regular expression to filter for.
#' @export

get_recent_file <- function(dir, pattern = NULL) {
  if(is.null(pattern)) pattern <- "\\.csv|\\.txt"
  files = list.files(dir, full.names = T, pattern = pattern)
  if(length(files) == 0) stop("No files found!", call. = F)
  times = file.mtime(files)
  recent = files[times == max(times)]
  if(length(recent) > 1) warning("More than one file found!", call. = F)
  recent
}
