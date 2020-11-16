#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

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

remove_na <- function(x) {
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
that <- base::which

#' Lines of R code
#'
#' How many lines of R code in a directory?
#'
#' @param x Directory
lines_of_r_code <- function(x) {
  sum(vapply(
    list.files(x, full.names = TRUE, recursive = TRUE, pattern = "\\.[rR]$"),
    function(xx) {
      tryCatch(length(readLines(xx)),
               error = function(e) NULL,
               finally = 0L)
    },
    integer(1)
  ))
}
