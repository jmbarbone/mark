#' String Slice
#'
#' Slice/split a string into multiple lines by the desired length of the line.
#'
#' @param x A character vector
#' @param n Integer, the length of the line split
#' @return A `character` vector
#'
#' @export
#' @examples
#' if (requireNamespace("stringi")) {
#'   x <- stringi::stri_rand_lipsum(1)
#'   str_slice(x)
#'   str_slice_by_word(x, n = 50L)
#' }

str_slice <- function(x, n = 80L) {
  ss <- chr_split(x)
  n_chars <- nchar(x)
  lens <- seq.int(1L, n_chars, by = n)

  vap_chr(
    lens,
    function(.x) {
      end <- .x + n - 1
      if (end > n_chars) {
        end <- n_chars
      }
      collapse0(ss[.x:end], sep = "")
    }
  )
}

#' @export
#' @rdname str_slice
str_slice_by_word <- function(x, n = 80L) {
  ss <- chr_split(x)
  n_chars <- nchar(x)

  st <- 1L
  ahead <- "a" # needs a dummy for start of loop
  starts <- integer()
  ends <- integer()

  while (st <= n_chars) {
    end <- st + n - 1L

    if (end > n_chars) {
      end <- n_chars
      ahead <- " "
    } else {
      ahead <- ss[end + 1L]
    }

    while (!grepl("[[:space:]]", ahead)) {
      end <- end - 1L
      ahead <- ss[end + 1L]
    }

    starts <- c(starts, st)
    ends <- c(ends, end)
    st <- end + 2L

  }

  mapply(
    function(xx, y) {
      collapse0(ss[xx:y], sep = "")
    },
    xx = starts,
    y = ends
  )
}

#' Extract date from string
#'
#' @param x A character vector
#' @param format A date format to find
#'
#' @return A `Date` (if found) or `NA`
#' @export
#' @examples
#' str_extract_date("This is a file name 2020-02-21.csv")
#' str_extract_date(c("This is a file name 2020-02-21.csv",
#'                    "Date of 2012-06-15 here"))
#' str_extract_date(c("This is a file name 2020-02-21.csv", "No date"))
#' str_extract_date("Last saved 17 December 2019", format = "%d %B %Y")
#'
#' str_extract_datetime(c("2020-02-21 235033", "2012-12-12 121212"))
#' str_extract_datetime("This is a file name 2020-02-21 235033.csv")
#'
str_extract_date <- function(x, format = "%Y-%m-%d") {
  frex <- format_to_regex(format)
  text <- string_extract(x, frex, ignore.case = TRUE)
  as.Date.character(text, format = format, optional = TRUE)
}

#' @rdname str_extract_date
#' @export
str_extract_datetime <- function(x, format = "%Y-%m-%d %H%M%S") {
  frex <- format_to_regex(format)
  text <- string_extract(x, frex, ignore.case = TRUE)
  capply(text, strptime, format = format, tz = "")
}

string_extract <- function(x, pattern, perl = FALSE, ignore.case = FALSE) { # nolint: object_name_linter, line_length_linter.
  re <- regexpr(pattern, x, perl = perl, ignore.case = ignore.case)
  starts <- as.vector(re, "integer")
  substr(x, starts, starts + attr(re, "match.length") - 1L)
}

#' Format string to a regular expression
#'
#' @param x A date or datetime format, assuming that entries follow the formats
#'  described in [base::strptime]
#'
#' @examples
#' mark:::format_to_regex("%Y-%m-%d")
#' mark:::format_to_regex("%b/%d/%y")
#' mark:::format_to_regex("%d %B %Y")
#'
#' pattern <- mark:::format_to_regex("%Y-%m-%d")
#' grepl(pattern, Sys.Date())
#' @noRd
format_to_regex <- function(x) {
  # may not need to be so comprehensive because a bad date will fail with the
  #   date parsing anyway
  x <- gsub("[[:space:](-/_:)]", ".", x)
  x <- sub("%Y", "([[:digit:]]{4})", x)
  # TODO maybe change this to use just the numbers?
  x <- sub("%b", month_abbr_regex, x)
  x <- sub("%B", month_name_regex, x)
  # These require leading 0s
  x <- sub("%d", "(0[1-9]|[12][0-9]|3[01])", x)
  x <- sub("%e", "(0[1-9]|1[0-9]|1[0-2])", x)
  x <- sub("%m", "(0[1-9]|1[0-9]|1[0-2])", x)
  x <- sub("%y", "([[:digit:]]{2})", x)
  x <- sub("%Y", "([[:digit:]]{4})", x)
  x <- sub("%H", "([01][0-9]|[2][0-4])", x)
  x <- sub("%M", "([0-5][0-9]|60)", x)
  x <- sub("%S", "([0-5][0-9]|60)", x)
  x
}

month_abbr_regex <- sprintf("(%s)", paste(month.abb,  collapse = "|"))
month_name_regex <- sprintf("(%s)", paste(month.name, collapse = "|"))

#' Character split
#'
#' Split apart a string by each character
#'
#' @param x A vector of strings to split
#' @return A `character` vector of length `nchar(x)`
#'
#' @examples
#' chr_split("split this")
#' @export
chr_split <- function(x) {
  stopifnot(length(x) == 1L)
  strsplit(as.character(x), "")[[1]]
}

#' Print as c
#'
#' Prints a vector to paste into an R script
#'
#' @details
#' This sorts (if set) and provides unique values for each element in `x` and
#'   prints then as a call to `c`.  This can be useful for copying data that you
#'   want to save as a vector in an R script.
#' The result is both called in `cat()` as well as copied to the clipboard.
#'
#' @param x A vector (defaults to reading the clipboard)
#' @param sorted If `TRUE` (default) applies `sort()` to `x`
#' @param null If `TRUE` (default) adds `NULL` at the end of the `c()` print
#' @return Invisibly, as a `character` vector, the object printed to the console
#' @examples
#' print_c(1:10)
#' print_c(letters[1:3])
#' print_c(month.abb)
#'
#' @export
print_c <- function(x = read_clipboard(), sorted = TRUE, null = TRUE) {
  check_is_vector(x)

  x <- unique(unlist(x))

  if (sorted) {
    x <- sort(x)
  }

  string <- sprintf('c(\n"%s",\nNULL\n)', collapse0(x, sep = '",\n"'))

  if (!null) {
    string <- gsub(",\nNULL", "", string)
  }

  if (is.numeric(x)) {
    string <- gsub('"', "", string)
  }

  if (interactive()) {
    write_clipboard(string) # nocovr
  }

  cat(string)
  invisible(string)
}
