#' @importFrom magrittr %>%
magrittr::`%>%`

# Like rlang::`%||%` but uses base is.null -- same thing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Colons
#'
#' Get an object from a package
#'
#' @details
#' This is a work around to calling `:::`.
#'
#' @section WARNING:
#' To reiterate from other documentation: it is not advised to use `:::` in
#'   your code as it will retrieve non-exported objects that may be more
#'   likely to change in their functionality that exported objects.
#'
#' @param package Name of the package
#' @param name Name to retrieve
#'
#' @export
`%colons%` <- function(package, name) {
  tryCatch(
    get(name, envir = asNamespace(package)),
    error = function(e) {
      stop(sprintf("`%s` not found in package `%s`",
                   name, package),
           call. = FALSE)
    }
  )
}

#' Parse and evaluate text
#'
#' A wrapper for eval(parse(text = .))
#'
#' @param x A character string to parse
#' @param envir The environment in which to evaluate the code
#' @export
ept <- function(x, envir = parent.frame()) {
  eval(parse(text = x), envir = envir)
}

construct_date <- function(date_text, sep = "") {
  collapse(vap_chr(unlist(strsplit(date_text, "")), date_switch), sep = sep)
}

date_switch <- function(x) {
  switch(x,
         Y = "[[:digit:]]{4}",
         y = "[0-99]{2}",
         m = "1[0-2]|[1-9]",
         b = "[[:alpha:]]{3,}",
         d = "3[01]|[12][0-9]|[1-9]",
         NULL)
}

# construct_date("ymd")
# construct_date("bdY")
# grepl(construct_date("bdY", "[\\s|,]+"), "December 17, 1992", ignore.case = TRUE)
# grepl(construct_date("Ymd", "-"), "1992-12-17", ignore.case = TRUE)

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


#' Character split
#'
#' Split apart a string by each character
#'
#' @param x A vector of strings to split
#' @export
chr_split <- function(x) {
  stopifnot("x must be a single element" = length(x) == 1)
  strsplit(as.character(x), "")[[1]]
}

is_length0 <- function(x) {
  length(x) == 0L
}
