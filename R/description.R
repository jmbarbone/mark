#' Add author to DESCRIPTION
#'
#' Adds author to description
#'
#' @details
#' Only valid for a single author.
#'
#' @param author_info Author information as a named list
#' @export

use_author <- function(author_info = find_author()) {
  stopifnot(
    "author_info must be a list" = is.list(author_info),
    "author_info should not be a person object" = !inherits(author_info, "person")
  )

  lines <- readLines("DESCRIPTION")
  start <- grep("^[Aa]uthor", lines)

  if (!length(start)) {
    stop("Jordan needs to review this, sorry")
    start <- grep("^[Vv]ersion", lines)
    lines <- c(lines[1:start], "Author: ", lines[(start + 1):length(lines)])
  }

  spaces <- grepl("^\\s", lines)
  end <- which.max(!spaces[-c(1:start)]) + start - 1

  names(author_info) <- tolower(names(author_info))
  nm <- names(author_info)
  valid_names <- c("given", "family", "middle", "email", "role", "comment")
  ok <- nm %in% valid_names & sapply(author_info, check_field)
  author_info <- author_info[ok]

  body <- author_info_to_text(author_info)
  n <- length(body)
  new_body <- c("Authors@R:",
                paste0("    person(", trimws(body[1], "left")),
                if (n > 2) paste0("           ", body[2:(n - 1)]) else NULL,
                paste0("           ", sub("[,]$", "", body[n]), ")"))

  out <- c(lines[1:(start - 1)],
           new_body,
           lines[(end + 1):length(lines)])

  writeLines(out, "DESCRIPTION")
}

check_field <- function(x) {
  is.character(x) && x != ""
}

author_info_to_text <- function(x) {
  nm <- names(x)
  width <- max(sapply(nm, nchar))

  comment <- nm == "comment"
  len <- sapply(x, length) == 1

  ind <- !comment & len
  x[ind] <- paste0('"', x[ind], '"')
  x[comment] <-  paste0("c(", names(x[comment][[1]]), " = ",  paste0('"', x[comment][1], '"'), ")")

  paste0(format(nm, width = width), " = ", x, ",")
}

find_author <- function() {
  author <- getOption("jordan.author")
  if (!is.null(author)) {
    return(author)
  }

  stop(
    "Author information not found in options.\n",
    "You can set the author information with options(jordan.author = ...)\n",
    "  probably within an .Rprofile",
    call. = FALSE
  )
}

#' Bump version (date)
#'
#' Bump package version for dates
#'
#' @description
#' Will read the DESCRIPTION file and adjust the version
#' Assumes that the version is written with major releases of 0 and appending
#'   the date.
#'
#' When the current version is the same as today's date, it will append a `.1`.
#' @export
bump_date_version <- function() {
  description <- readLines("DESCRIPTION")
  line <- grep("^[Vv]ersion.*[[:punct:][:digit:]]+$", description)

  stopifnot(length(line) == 1L)

  v <- gsub("[Vv]ersion|[:]|[[:space:]]", "", description[line])
  version <- package_version(v)
  today <- today_as_version()


  new <- if (version == today) {
    do_bump_date_version(version, add = TRUE)
  } else if (version > today) {
    do_bump_date_version(version)
  } else {
    today
  }

  message(sprintf("Version updated: %s to %s", version, new))

  description[line] <- sprintf("Version: %s", new)
  writeLines(description, "DESCRIPTION", sep = "\n")
}

# Some redundancy here
# What about just packageVersion() ?

version_update <- function(version) {
  description <- readLines("DESCRIPTION")
  line <- grep("^[Vv]ersion.*[[:punct:][:digit:]]+$", description)

  stopifnot(length(line) == 1L)

  old <- gsub("[Vv]ersion|[:]|[[:space:]]", "", description[line])

  message(sprintf("Version updated: %s to %s", old, version))
  description[line] <- sprintf("Version: %s", version)
  writeLines(description, "DESCRIPTION", sep = "\n")
}

do_bump_date_version <- function(version, add = FALSE) {
  stopifnot(is.package_version(version))

  x <- unclass(version)[[1]]
  n <- length(x)

  if (add) {
    x[n + 1] <- 1
  } else {
    x[n] <- x[n] + 1
  }

  package_version(collapse0(x, sep = "."))
}


today_as_version <- function() {
  x <- unclass(as.POSIXlt(Sys.Date()))
  char <- paste(0, x[["year"]] + 1900, x[["mon"]] + 1, x[["mday"]], sep = ".")
  as.package_version(char)
}
