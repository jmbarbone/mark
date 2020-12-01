#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

`%colons%` <- function(pkg, name) {
  get(name, envir = asNamespace(pkg))
}

ept <- function(x) {
  eval(parse(text = x))
}

# Happily ripped from: http://r-pkgs.had.co.nz/description.html
require_namespace <- function(namespace) {
  if (!rn(namespace)) {
    stop(paste("Package <<", namespace, ">> needed for this function to work."),
         call. = FALSE)
  }
}

rn <- function(namespace) {
  requireNamespace(namespace, quietly = TRUE)
}

rn_soft <- function(namespace) {
  if (!rn(namespace)) {
    quiet_stop()
  }
}

quiet_stop <- function() {
  op <- options()
  options(show.error.messages = FALSE)
  stop()
  on.exit(options(op), add = TRUE)
}

construct_date <- function(date_text, collapse = "") {
  paste(sapply(unlist(strsplit(date_text, "")),
               date_switch,
               USE.NAMES = FALSE),
        collapse = collapse)
}

date_switch <- function(x) {
  switch(x,
         Y = "[[:digit:]]{4}",
         y = "[0-99]{2}",
         m = "1[0-2]|[1-9]  ",
         b = "[[:alpha:]]{3,}",
         d = "3[01]|[12][0-9]|[1-9]",
         NULL)
}

# construct_date("ymd")
# construct_date("bdY")
# grepl(construct_date("bdY", "[\\s|,]+"), "December 17, 1992", ignore.case = TRUE)
# grepl(construct_date("Ymd", "-"), "1992-12-17", ignore.case = TRUE)

# Simplified as factor
# Does not sort the levels
as_factor_unordered <- function(f) {
  factor(f, levels = unique(as.character(f)))
}

#' Remove NA
#'
#' Remove NAs from a vector
#'
#' @param x A vector
#'
#' @export
remove_na <- function(x) {
  stopifnot(is.vector(x))
  x[!is.na(x)]
}

unique_no_na <- function(x) {
  unique(remove_na(x))
}

is_named <- function(x) {
  !is.null(names(x))
}

#' Sort by names
#'
#' Sort a vector by it's name
#'
#' @param x A vector
#'
#' @export
sort_names <- function(x) {
  stopifnot(is_named(x), is.vector(x))
  x[sort(names(x))]
}

# Removes object's attributes before printing
print_no_attr <- function(x, ...) {
  attributes(x) <- NULL
  NextMethod("print", x)
}

#' That
#'
#' Grammatical correctness
#'
#' @details
#' See `fortunes::fortune(175)`.
#'
#' @inheritParams base::which
#'
#' @export
#' @seealso [base::which()]
that <- function(x, arr.ind = FALSE, useNames = TRUE) {
  which(x, arr.ind = arr.ind, useNames = useNames)
}

#' Lines of R code
#'
#' Find the total number of lines of R code
#'
#' @details
#' Tries to read each file in the directory that ends in .R or .r and sums
#'   together.  Files that fail to read are not counted.
#'
#' @param x Directory to search for files
#' @param skip_empty Logical, if TRUE will not count lines that are empty or
#'   only contain a bracket or quotation mark.
#'
#' @export
#'
#' @examples
#' lines_of_r_code(system.file())
#' lines_of_r_code(system.file(), skip_empty = FALSE)

lines_of_r_code <- function(x = ".", skip_empty = TRUE) {
  if (dir.exists(x)) {
    files <- list_r_files(x)
  }

  if (skip_empty) {
    return(sum(vap_int(files, n_lines_r_file)))
  } else {
    sum(vap_int(files, n_lines_r_file_all))
  }
}

list_r_files <- function(x = ".") {
  list_files(x, pattern = "\\.[rR]$", all = TRUE)
}

n_lines_r_file <- function(r_file) {
  withCallingHandlers({
    x <- trimws(readLines(r_file, skipNul = TRUE, warn = FALSE))
    length(x %wo% c("", "}", "{", "(", ")", "[", "]", '"', "'"))
  },
  error = function(e) {
    return(0L)
  })
}

n_lines_r_file_all <- function(r_file) {
  withCallingHandlers({
    length(readLines(r_file, skipNul = TRUE, warn = FALSE))
  },
  error = function(e) {
    return(0L)
  })
}

chr_split <- function(x) {
  stopifnot("x must be a single element" = length(x) == 1)
  strsplit(as.character(x), "")[[1]]
}

# vaps --------------------------------------------------------------------

# I'm lazy and don't want to use a bunch of exports and I like the functionality
#   of map_* so this is shorthand and only base dependent


# These are single element expectations

vap_int <- function(x, f, ..., nm = FALSE) {
  vapply(x, FUN = f, FUN.VALUE = integer(1), ..., USE.NAMES = nm)
}

vap_dbl <- function(x, f, ..., nm = FALSE) {
  vapply(x, FUN = f, FUN.VALUE = double(1), ..., USE.NAMES = nm)
}

vap_chr <- function(x, f, ..., nm = FALSE) {
  vapply(x, FUN = f, FUN.VALUE = character(1), ..., USE.NAMES = nm)
}

# dates are expected to be in a standard format
vap_date <- function(x, f, ..., nm = FALSE) {
  out <- vap_dbl(x, f, ..., nm = nm)
  as.Date.numeric(out, origin = "1970-01-01")
}

# sapply(1:5, function(x) Sys.Date() + x)
# capply(1:5, function(x) Sys.Date() + x)

# This can take multiple elements, so can be a little dangerous
capply <- function(x, f, ..., nm = FALSE) {
  do.call(c, sapply(x, FUN = f, ..., simplify = FALSE, USE.NAMES = nm))
}
