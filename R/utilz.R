#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# Smaller functions that are used internally

deparser <- function(x, env = parent.frame()) {
  if(class(substitute(x, env)) == "name") deparse(substitute(x, env)) else x
}


# Happily ripped from: http://r-pkgs.had.co.nz/description.html

require_namespace <- function(namespace) {
  if (!requireNamespace(namespace, quietly = TRUE)) {
    stop(sprintf("Package \"%s\" needed for this function to work. Please install it.", namespace),
         call. = FALSE)
  }
}

opposite <- function(x, y) {
  all(x != y)
}

all_na <- function(x, ...) {
  UseMethod("all_na", x)
}

all_na.default <- function(x) {
  all(is.nan(x) | is.na(x))
}

all_na.character <- function(x, convert = FALSE) {
  found <- which(x == "NaN")
  if(convert) {
    x[found] <- NA_character_
  } else if(length(found) > 0) {
    warning("These values may be NA types `convert`ed to character:\n",
            paste(paste0("    ",  x[found]), collapse = "\n"),
            call. = FALSE)
  }
  all_na.default(x)
}

# all_na(c(NA, NaN, NA_complex_, NA_integer_))
# all_na(c(NA, NA_character_))
# all_na(c(NA, NA_character_, NA_complex_, NA_integer_))
# all_na(c(NA_character_, NaN)) ## fails
# all_na(c(NA_character_, NaN), override_nan = TRUE)


construct_date <- function(date_text, collapse = "") {
  paste(sapply(unlist(strsplit(date_text, "")), date_switch, USE.NAMES = FALSE), collapse = collapse)
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

