#' Read Bib file
#'
#' Read a bib file into a data.frame
#'
#' @details
#' Inspired and partially credited to `bib2df::bib2df()` although this has no
#'   dependencies outside of base functions and much quicker.  This speed seems
#'   to come from removing `stringr` functions and simplifying a few *apply
#'   functions.
#' This will also include as many categories as possible from the entry.
#'
#' @param file File or connection
#' @param skip The lines to skip
#' @param max_lines The maximum number of lines to read
#' @param encoding Assumed encoding of file (passed to readLines)
#'
#' @return A `data.frame` with each row as a bib entry and each column as a
#'   field
#' @seealso [bib2df::bib2df()]
#'
#' @export
#'
#' @examples
#' file <- "https://raw.githubusercontent.com/jmbarbone/bib-references/master/references.bib"
#' bibdf <- read_bib(file, max_lines = 51L)
#'
#' if (package_available("tibble")) {
#'   tibble::as_tibble(bibdf)
#' } else {
#'   head(bibdf)
#' }
#'
#' if (package_available("bib2df") & package_available("bench")) {
#'   file <- system.file("extdata", "bib2df_testfile_3.bib", package = "bib2df")
#'
#'   # Doesn't include the 'tidying' up
#'   foo <- function(file) {
#'     bib <- ("bib2df" %colons% "bib2df_read")(file)
#'     ("bib2df" %colons% "bib2df_gather")(bib)
#'   }
#'
#' \donttest{
#'   bench::mark(
#'     read_bib = read_bib(file),
#'     bib2df = bib2df::bib2df(file),
#'     foo = foo(file),
#'     check = FALSE
#'   )[1:9]
#' }
#' }

read_bib <- function(file, skip = 0L, max_lines = NULL, encoding = "UTF-8") {
  # Account for nul values found in encoding?
  # skipNul = TRUE could do this but an error can still be caused later
  bib <- readLines(file, encoding = encoding, skipNul = FALSE)

  if (!is.null(max_lines)) {
    start <- skip + 1L
    n_lines <- length(bib)
    bib <- bib[seq.int(start, min(skip + max_lines, n_lines))]
  }

  bib <- gsub("[^[:graph:]]", " ", bib)

  # Find start of entries
  from <- grep("[@]", bib)

  if (!length(from)) {
    stop("No entries detected", call. = FALSE)
  }

  # shift over (may contain white space?)
  to <- c(from[-1L] - 1L, length(bib))

  item_list <- Map(function(x, y) bib[x:y], x = from, y = to - 1L)

  # Extract first line for speediness
  first_line <- sapply(item_list, `[`, 1L)

  keys <- gsub(".*((?<=\\{)[^,]+).*", "\\1", first_line, perl = TRUE)
  fields <- gsub(".*((?<=@)[^\\{]+).*", "\\1", first_line, perl = TRUE)
  fields <- tolower(fields)

  # TODO Implement checks for duplicate categories?
  # categories <- lapply(item_list, get_bib_categories)
  # values <- lapply(item_list, get_bib_values)
  out <- lapply(item_list, parse_bib)
  categories <- lapply(out, "[[", "cat")
  values <- lapply(out, "[[", "val")

  as_bib(
    process_bib_dataframe(categories, values, fields, keys),
    bib_list = process_bib_list(keys, fields, categories, values)
  )
}

# FUNS --------------------------------------------------------------------

parse_bib <- function(x) {
  x <- x[-1L]
  m <- regexpr("=", x)
  ok <- m > 0L
  m <- m[ok]
  x <- x[ok]
  res <- list(
    cat = substring(x, 1L, m - 1L),
    val = substring(x, m + 1L, nchar(x))
  )
  res$cat <- parse_bib_cat(res$cat)
  res$val <- parse_bib_val(res$val)
  res
}

parse_bib_cat <- function(x) {
  x <- trimws(x)
  x[x %in% c("{", "}")] <- NA_character_
  tolower(x)
}

parse_bib_val <- function(x) {
  x[!lengths(x)] <- NA_character_
  # There may be something better than this
  # Would like to maintain the { and }
  # x <- gsub("\\{|\\}|,?$", "", x)
  x <- trimws(x)
  x <- gsub("^(\\{|\")|(\"|\\})[,]?$", "", x)
  x <- gsub(",$", "", x)
  x
}

#' Process bib values
#'
#' Generates a data frame of values from bibs
#'
#' @param categories A list of categories
#' @param values A list of values
#' @param fields a Vector of fields
#' @param keys a Vector of keys
#'
#' @return A wide `data.frame` with explicit `NA`s

process_bib_dataframe <- function(categories, values, fields, keys) {
  # Determine all categories for missing values inside Map
  ucats <- unique(remove_na(unlist(categories)))
  ucats_df <- quick_dfl(category = ucats, value = rep(NA_character_, length(ucats)))

  x <- mapply(
    function(cats, vals, field, key) {
      # Determine valid categories/values
      valids <- !is.na(cats)
      cats <- cats[valids]
      vals <- vals[valids]

      # Check for duplicate categories
      lens <- counts(cats)
      bad <- lens > 1L

      if (any(bad)) {
        msg <- sprintf("The key `%s` has duplicate categories of `%s`", key, names(lens)[bad])
        stop(simpleError(msg))
      }

      # Append vectors
      cats <- c("key", "field", cats)
      vals <- c(key, field, vals)

      # Create data.frame
      data <- quick_dfl(category = cats, value = vals)

      # Check for missing categories
      toadd <- ucats %out% cats
      if (any(toadd)) {
        data <- rbind(data, ucats_df[toadd, ])
      }

      # Transpose to prep for reduce rbinding
      quick_df(set_names0(as.list(data[[2L]]), data[[1L]]))
    },
    cats = categories,
    vals = values,
    field = fields,
    key = keys,
    SIMPLIFY = FALSE
  )

  # TODO consider mark::row_bind(), when ready
  Reduce(rbind, x)[, c("key", "field", ucats), ]
}

process_bib_list <- function(keys, fields, categories, values) {
  valid <- !is.na(values) & values != ""

  x <- mapply(
    function(key, field, cats, vals) {
      # # vals[is.na(vals)] <- ""
      # new <- c(key, field, vals)
      # names(new) <- c("key", "field", cats)
      # class(new) <- c("character", "mark_bib_entry", .Names = key)
      # new
      struct(
        c(key, field, vals),
        class = c("character", "mark_bib_entry"),
        names = c("key", "field", cats)
      )

    },
    key = keys[valid],
    field = fields[valid],
    cats = categories[valid],
    vals = values[valid],
    SIMPLIFY = FALSE
  )

  as_bib_list(x, keys[valid])
}

as_bib_list <- function(x, names = NULL) {
  if (!is.list(x)) {
    stop("`x` must be a list", call. = FALSE)
  }
  class(x) <- c("list", "mark_bib_list")
  x
}

as_bib <- function(x, bib_list = NULL) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data.frame", call. = FALSE)
  }

  class(x) <- c("mark_bib_df", "data.frame")
  attr(x, "bib_list") <- bib_list
  x
}

# Prints ------------------------------------------------------------------

#' @export
print.mark_bib_list <- function(x, ...) {
  nm <- names(x)
  out <- x
  names(out) <- NULL

  # TODO run format() on all categories for equal lengths
  for (i in seq_along(out)) {
    nmi <- nm[i]
    bordern <- getOption("width") - nchar(nmi) - 1L
    header <- if (bordern) {
      paste0(" ", collapse0(rep("-", bordern), sep = ""))
    } else {
      ""
    }

    co <- utils::capture.output(print(out[[i]]))[-1L]

    cat(
      paste0(if (i == 1L) "" else "\n", nmi, header),
      collapse0(paste0("  ", co), sep = "\n"),
      sep = "\n"
    )
  }

  invisible(x)
}

#' @export
print.mark_bib_entry <- function(x, ...) {
  out <- x[!(is.na(x) | x == "")]
  nm <- names(out)
  chars <- nchar(nm)
  chars[is.na(chars)] <- 0L
  chars_max <- max(chars)

  blanks <- vap_chr(
    chars_max - chars + 2L,
    function(x) {
      collapse0(rep(" ", x), sep = "")
    }
  )

  cat(paste(nm, blanks, out), sep = "\n")
  invisible(x)
}

#' Print bib data frame
#'
#' Print bib dataframe, or as list
#'
#' @param x The `mark_bib_df` object
#' @param list If `TRUE` will print as a list rather than the `data.frame`
#' @param ... Additional arguments passed to methods
#' @returns `x`, invisibly, called for its side effects
#' @export
print.mark_bib_df <- function(x, list = FALSE, ...) {
  if (list) {
    print(attr(x, "bib_list"), ...)
  } else {
    y <- x
    attr(y, "bib_list") <- NULL
    NextMethod()
    # print(y, ...)
  }

  invisible(x)
}
