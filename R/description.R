#' Add author to DESCRIPTION
#'
#' Adds author to description
#'
#' @details
#' Only valid for a single author.
#'
#' @param author_info Author information as a named list
#' @return None, called for side effects
#' @export

use_author <- function(author_info = find_author()) {
  stopifnot(
    is.list(author_info),
    !inherits(author_info, "person")
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
  valid_names <- c("given", "family", "middle", "email", "role", "comment")
  ok <- names(author_info) %in% valid_names & vap_lgl(author_info, check_field)
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
  is.character(x) && length(x) >= 1L && all(nzchar(x), na.rm = TRUE)
}

author_info_to_text <- function(x) {
  nm <- names(x)
  width <- max(sapply(nm, nchar))

  comment <- nm == "comment"
  len <- lengths(x) == 1

  ind <- !comment & len
  x[ind] <- paste0('"', x[ind], '"')
  x[comment] <-  paste0(
    "c(",
    names(x[comment][[1]]),
    " = ",
    paste0('"', x[comment][1], '"'), ")"
  )

  paste0(format(nm, width = width), " = ", x, ",")
}

find_author <- function() {
  getOption("mark.author", stop(cond_find_author()))
}

# Version -----------------------------------------------------------------

#' Get and bump version
#'
#' Get and bump package version for dates
#'
#' @description Will read the `DESCRIPTION` file and to get and adjust the
#' version
#'
#' `bump_date_version()` will not check if the version is actually a date.  When
#' the current version is the same as today's date(equal by character strings)
#' it will append a `.1`.
#'
#' @param version A new version to be added; default of `NULL` will
#'   automatically update.
#' @param date If `TRUE` will use a date as a version.
#' @return
#' * `get_version()`: A package_version
#' * `bump_version()`: None, called for its side-effects
#' * `bump_date_version()`: None, called for its side-effects
#' * `update_version()`: None, called for its side-effects
#'
#' @export
get_version <- function() {
  description <- readLines("DESCRIPTION")
  line <- grep("^[Vv]ersion.*[[:punct:][:digit:]]+$", description)

  if (length(line) != 1L) {
    stop(cond_version_lines())
  }

  as.package_version(gsub("[Vv]ersion|[:]|[[:space:]]", "", description[line]))
}

#' @export
#' @rdname get_version
bump_version <- function(version = NULL) {
  update_version(version)
}

#' @export
#' @rdname get_version
bump_date_version <- function(version = NULL) {
  update_version(version, date = TRUE)
}

#' @export
#' @rdname get_version
update_version <- function(version = NULL, date = FALSE) {
  # Get the DESCRIPTION
  description <- readLines("DESCRIPTION")

  # Identify the correct line
  line <- grep("^[Vv]ersion.*[[:punct:][:digit:]]+$", description)

  if (length(line) != 1L) {
    stop(cond_version_lines())
  }

  # Get the old version
  old <- gsub("[Vv]ersion|[:]|[[:space:]]", "", description[line])

  # If new isn't passed update by date or by version
  version <- if (!is.null(version)) {
    version
  } else if (date) {
    do_bump_date_version(old)
  } else {
    do_bump_version(old)
  }

  if (!inherits(version, "package_version")) {
    version <- as.package_version(collapse0(version, sep = "."))
  }

  foo <- function() {
    description[line] <- sprintf("Version: %s", version)
    invisible(writeLines(description, "DESCRIPTION", sep = "\n"))
  }

  # Use menu to check if updates are fine
  # nocov start
  men <- if (check_interactive()) {
    utils::menu(
      title = sprintf("Update version from %s to %s?", old, version),
      choices = c("yes", "no")
    )
  }
  # nocov end

  if (identical(men, 1L) || isNA(getOption("mark.check_interactive"))) {
    foo()
  }

  invisible(NULL)
}

do_bump_version <- function(version) {
  x <- unclass(as.package_version(version))[[1]]
  n <- length(x)
  x[n] <- x[n] + 1
  x
}

do_bump_date_version <- function(version) {
  today <- today_as_version(chr_split(version)[1] == "0")

  if (version < today) {
    return(today)
  }

  version <- as.package_version(version)

  x <- unclass(version)[[1]]
  n <- length(x)

  if (version == today) {
    x[n + 1] <- 1
  } else {
    x[n] <- x[n] + 1
  }

  package_version(collapse0(x, sep = "."))
}

# Some redundancy here
# What about just packageVersion() ?

today_as_version <- function(zero = FALSE) {
  x <- unclass(as.POSIXlt(Sys.Date()))

  char <- if (zero) {
    paste(0, x[["year"]] + 1900, x[["mon"]] + 1, x[["mday"]], sep = ".")
  } else {
    paste(   x[["year"]] + 1900, x[["mon"]] + 1, x[["mday"]], sep = ".") # nolint: spaces_inside_linter, line_length_linter.
  }
  as.package_version(char)
}

# conditions --------------------------------------------------------------

cond_find_author <- function() {
  new_condition(
    paste0(
      "Author information not found in options.\n",
      "You can set the author information with options(mark.author = .)\n",
      "  probably within an .Rprofile"
    ),
    "find_author"
  )
}

cond_version_lines <- function() {
  new_condition(
    "multiple versions found",
    "get_version_lines"
  )
}

# terminal line
