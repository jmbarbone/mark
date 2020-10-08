#' String Close enough
#'
#' Find a value that is, eh, close enough by listed all possible values.
#' It's not the most precise but can be used to determine additional explortation.
#'
#' @param string Input vector. Either a character vector, or something coercible to one.
#' @param pattern A character string of the actual value (see below)
#' @param negate If TRUE, return non-matching elements.
#'
#' @details pattern will be coerced like so: "abc" turned into "[a|b|c][a|b|c][a|b|c]"
#'
#' @export
#'
#' @examples
#' x <- c("thsi", "TIHS", "that")
#' str_close_enough(x, "this", negate = FALSE) ##  TRUE  TRUE FALSE
#' str_close_enough(x, "this", negate = TRUE)  ## FALSE FALSE  TRUE

str_close_enough <- function(string, pattern, negate = FALSE) {
  temp <- unlist(strsplit(pattern, ""))
  unit <- paste(temp, collapse = "|")
  find <- paste0("[", paste(rep(unit, length(temp)), collapse = "]["), "]")
  res <- grepl(pattern = find, x = string, ignore.case = TRUE)

  if (negate) {
    !res
  } else {
    res
  }
}


#' String Slice
#'
#' Slice/split a string into multiple lines by the desired length of the line.
#'
#' @param x A character vector
#' @param n Integer, the length of the line split
#'
#' @export
#' @examples
#' if (requireNamespace("stringi")) {
#'   x <- stringi::stri_rand_lipsum(1)
#'   str_slice(x)
#'   str_slice_by_word(x, n = 50L)
#' }

str_slice <- function(x, n = 80L) {
  stopifnot(is.character(x))

  ss <- strsplit(x, "")[[1]]
  n_chars <- nchar(x)
  lens <- seq(1L, n_chars, by = n)

  vapply(lens,
         function(x) {
           end <- x + n - 1
           if (end > n_chars) {
             end <- n_chars
           }
           paste(ss[seq(x, end, by = 1L)], collapse = "")
         },
         character(1))

}

#' @export
#' @rdname str_slice
str_slice_by_word <- function(x, n = 80L) {
  stopifnot(is.character(x),
            "Input text must be length 1L" = length(x) == 1L)

  ss <- strsplit(x, "")[[1]]
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

    starts <- append(starts, st)
    ends <- append(ends, end)
    st <- end + 1L
  }

  mapply(function(x, y) paste(ss[x:y], collapse = ""),
         x = starts,
         y = ends)

}


#' Extract date from string
#'
#' @param x A character vector
#' @param format A date format to find
#'
#' @return A date (if found) or NA
#' @export
#' @examples
#' str_extract_date("This is a file name 2020-02-21.csv")
#' str_extract_date(c("This is a file name 2020-02-21.csv", "No date"))
#' str_extract_date("Last saved 17 December 2019", format = "%d %B %Y")
str_extract_date <- function(x, format = "%Y-%m-%d") {
  frex <- format_to_regex(format)
  pattern <- sprintf(".*(%s).*", frex)
  text <- gsub(pattern, "\\1", x, ignore.case = TRUE)
  as.Date(text, format = format, optional = TRUE)
}


#' Format string to a regular expression
#'
#' @param x A date format, assuming
#' @examples
#' format_to_regex("%Y-%m-%d")
#' format_to_regex("%b/%d/%y")
#' format_to_regex("%d %B %Y")
#'
#' pattern <- format_to_regex("%Y-%m-%d")
#' grepl(pattern, Sys.Date())
format_to_regex <- function(x) {
  # may not need to be so comprehensive because a bad date will fail with the
  #   date parsing anyway
  x <- gsub("[[:space:](-/_)]", ".", x)
  x <- sub("%Y", "[[:digit:]]{4}", x)
  x <- sub("%b", month_abbr_regex, x)
  x <- sub("%B", month_name_regex, x)
  # TODO more comprehensive with (0)-31
  x <- sub("%d", "[[:digit:]]{2}", x)
  x <- sub("%e", "[[:digit:]]{1}", x)
  # TODO more comprehensive with (0)1-12
  x <- sub("%[em]", "[[:digit:]]{1,2}", x)
  x <- sub("%y", "[[:digit:]]{2}", x)
  x <- sub("%Y", "[[:digit:]]{4}", x)
  x
}

month_abbr_regex <- sprintf("(%s)", paste(month.abb,  collapse = "|"))
month_name_regex <- sprintf("(%s)", paste(month.name, collapse = "|"))
