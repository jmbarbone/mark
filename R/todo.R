#' Get TODOs
#'
#' Search for # TODO tags
#'
#' @details
#' Calls `git grep -in "[#] TODO"` to find any lines with a comment.
#' Removes any finds in the NAMESPACE
#'
#' @return `NULL` if none are found, otherwise a data.frame with the line
#'   number, file name, and TODO comment.
#'
#' @export

todos <- function() {
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

  if (length(finds) == 0) {
    message("No TODOs found")
    return(invisible())
  }

  splits <- strsplit(finds, ":")
  out <- Reduce(rbind, lapply(splits, clean_todo_split))

  tibble::as_tibble(out)
}

clean_todo_split <- function(x) {
  n <- length(x)
  stopifnot(n >= 3)

  if (n > 3) {
    x[3] <- paste(x[3:n], collapse = ":")
    x <- x[1:3]
  }

  names(x) <- c("file", "line", "todo")
  x <- as.list(x[c(2, 1, 3)])
  x["todo"] <- sub(".*[#]\\s{0,}TODO[:]?\\s", "", x["todo"])
  x["line"] <- as.integer(x["line"])

  structure(x, class = "data.frame", row.names = 1L)
}

# bench::mark(
#   `1` = vap_int(1:100, function(x) x + 1),
#   `2` = purrr::map_int(1:100, function(x) x + 1),
#   `3` = purrr::map_int(1:100, ~x + 1)
# )
