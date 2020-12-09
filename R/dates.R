#' Partial dates
#'
#' Derive a date object from a partial date
#'
#' @description
#' Takes a character as an argument and attempts to create a date object when
#'   part of the date string is missing.
#' @param x A vector of character dates
#' @param format Format order of the date (accepts only combinations of 'y',
#'   'm', and 'd')
#' @param method Method for reporting partial dates -- "min" or "max"
#' @examples
#' x <- c("2020-12-17", NA_character_, "", "2020-12-UN", "2020-12-UN",
#'        "2019-Unknown-00", "UNK-UNK-UNK", "1991-02-UN", "    ",
#'        "2020January20")
#' data.frame(x = x,
#'            min = date_from_partial(x),
#'            max = date_from_partial(x, method = "max"))
#'@export


date_from_partial <- function(x, format = "ymd", method = c("min", "max")) {
  x <- as.character(x)
  fmt <- verify_format(format)
  method <- match_param(method, c("min", "max"))

  out <- not_available("date", length(x))

  # disregard the bad inputs
  ok <- is_valid_date_string(x)

  if (!any(ok)) {
    return(out)
  }

  prep <- prep_date_string(x[ok])
  res <- as_date_strptime(prep, format = strp_format(fmt))
  nas <- is.na(res)

  if (length(nas) && any(nas)) {
    res[nas] <- parse_date_strings(prep[nas], fmt = fmt, method = method)
  }

  # replace only bad results
  out[ok] <- res
  out
}

verify_format <- function(format) {
  s <- chr_split(format)
  m <- match(c("y", "m", "d"), s)
  stopifnot(
    "format must be 3 characters" = length(unique(s)) == 3L,
    'format must contain "y", "m", and "d"' = !anyNA(m)
  )
  s
}

is_valid_date_string <- function(x) {
  !OR(
    is.na(x),
    x == "",
    !(grepl("[[:digit:]]+", x) | grepl("(.UNK?N?.?)", x)), # need some digits
    grepl("^([[:blank:]]|[[:punct:]]|[[a-zA-Z]]|[[:digit:]]){1,}$", x)
  ) | grepl("^[[:digit:]]{4}$", x)
}

prep_date_string <- function(x) {
  out <- toupper(x)
  out <- gsub("([0-9])([A-Z])", "\\1-\\2", out)
  out <- gsub("([A-Z])([0-9])", "\\1-\\2", out)
  out <- gsub("-+|[[:space:]]", "-", out)

  nums <- formatC(1:12, width = 2, flag = "0")

  for (i in 1:12) {
    out <- sub(month.NAME[i], nums[i], out)
    out <- sub(month.ABBR[i], nums[i], out)
  }

  out
}

parse_date_strings <- function(.x, fmt, method) {
  min_year <- getOption("jordan.min_year", 0L)
  max_year <- getOption("jordan.max_year", 9999L)

  splits <- strsplit(.x, "-")

  mat <- sapply(splits, function(x) {
    x <- switch(
      length(x),
      c(y = x, m = NA_character_, d = NA_character_),
      c(date_offset_match(x, fmt), d = NA_character_),
      set_names(x, fmt)
    )

    if (is.null(x)) {
      return(NA_Date_)
    }

    x <- set_names(suppressWarnings(as.integer(x)), names(x))
    x <- x[c('y', 'm', 'd')]
    x[is.na(x)] <- 0L

    if (all(x == integer(3))) {
      out <- switch(
        method,
        min = c(y = min_year, m = 1L, d = 1L),
        max = c(y = max_year, m = 12L, d = 31L)
      )

      return(out)
    }

    if (method == "min") {

      if (x['d'] == 0L) {
        x['d'] <- 1L
      }

      if (x['m'] == 0L) {
        x['m'] <- 1L
      }

      if (x['y'] == 0L) {
        x['y'] <- min_year
      }

      return(x)
    }

    if (x['m'] == 0L) {
      x['m'] <- 12L
    }

    if (x['d'] == 0L) {
      x['d'] <- days_in_month[x['m']]

      if (x['m'] == 2L && is_leap_year(x['y'])) {
        x['d'] <- 29L
      }
    }

    x
  },
  simplify = TRUE,
  USE.NAMES = FALSE)

  res <- sprintf(
    "%s-%s-%s",
    formatC(mat["y", ], width = 4L, flag = "0"),
    formatC(mat["m", ], width = 2L, flag = "0"),
    formatC(mat["d", ], width = 2L, flag = "0")
  )

  as_date_strptime(res)
}

# When only 2 date splits are found, assume year and month
date_offset_match <- function(x, fmt) {
  mt <- match(c('y', 'm', 'd'), fmt)
  names(mt) <- c('y', 'm', 'd')

  if (mt['d'] == 1L) {
    mt <- mt - 1L
  }

  mt <- mt[c('y', 'm')]
  set_names(x[mt], nms = c("y", "m"))
}

days_in_month <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
names(days_in_month) <- month.name
month.NAME <- toupper(month.name)
month.ABBR <- toupper(month.abb)

is_leap_year <- function(year = Sys.time()) {
  if (inherits(year, c("Date", "POSIXct", "POSIXlt"))) {
    year <- as.POSIXlt(year)$year + 1900
  }

  if (year %% 4 != 0) {
    FALSE
  } else if (year %% 100 != 0) {
    TRUE
  } else if (year %% 400 != 0) {
    FALSE
  } else {
    TRUE
  }
}

as_date_strptime <- function(x, format = "%Y-%m-%d") {
  as.Date(strptime(x, format = format, tz = getOption("jordan.tz", "GMT")),
          format = format, optional = TRUE)
}

strp_format <- function(fmt) {
  fmt[fmt == 'y'] <- "Y"
  sprintf("%%%s-%%%s-%%%s", fmt[1], fmt[2], fmt[3])
}
