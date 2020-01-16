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

all_na <- function(x) {
  all(is.nan(x) || is.na(x))
}
# all_na(c(NA, NaN, NA_character_, NA_complex_))


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

