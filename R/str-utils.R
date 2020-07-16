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
#' str_close_enough(c("thsi", "TIHS", "that"), "this", negate = TRUE)  ##  TRUE  TRUE FALSE
#' str_close_enough(c("thsi", "TIHS", "that"), "this", negate = FALSE) ## FALSE FALSE  TRUE

str_close_enough <- function(string, pattern, negate = FALSE) {
  temp <- unlist(strsplit(pattern, ""))
  unit <- paste(temp, collapse = "|")
  find <- paste0("[", paste(rep(unit, length(temp)), collapse = "]["), "]")
  res <- grepl(pattern = find, x = string, ignore.case = TRUE)
  if(negate) return(.Primitive("!")(res)) else res
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
  stopifnot(is.character(x))

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
