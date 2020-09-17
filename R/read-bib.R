#' Read Bib file
#'
#' Read a bib file into a data.frame
#'
#' @details
#' Inspired and partially credited to [bib2df::bib2df()] although this has no
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
#' @export
#'
#' @examples
#' file <- "https://raw.githubusercontent.com/jmbarbone/bib-references/master/references.bib"
#' bibdf <- read_bib(file, max_lines = 51L)
#'
#' \dontrun{
#' if (requireNamespace("tibble")) tibble::as_tibble(bibdf) else head(bibdf)
#'
#' if (requireNamespace("bib2df") & requireNamespace("microbenchmark")) {
#'   file1 <- system.file("extdata", "bib2df_testfile_3.bib", package = "bib2df")
#'
#'   # Doesn't include the 'tidying' up
#'   foo <- function(file) {
#'     bib <- bib2df:::bib2df_read(file)
#'     bib2df:::bib2df_gather(bib)
#'   }
#'
#'   microbenchmark::microbenchmark(
#'     read_bib(file1),
#'     bib2df::bib2df(file1),
#'     foo(file1)
#'   )
#' }
#' }

read_bib <- function(file, skip = 0, max_lines = NULL, encoding = "UTF-8")
{
  # Account for nul values found in encoding?
  # skipNul = TRUE could do this but an erro can still be caused later
  bib <- readLines(file, encoding = encoding, skipNul = FALSE)

  if (!is.null(max_lines)) {
    start <- skip + 1
    n_lines <- length(bib)
    last_line <- skip + max_lines
    bib <- bib[seq.int(start, ifelse(last_line > n_lines, n_lines, last_line))]
  }

  bib <- gsub("[^[:graph:]]", " ", bib)

  # Find start of entries
  from <- grep("[@]", bib)
  stopifnot("No entries detected" = length(from) > 0)

  # shift over (may contain white space?)
  to <- c(from[-1] - 1, length(bib))

  item_list <- Map(function(x, y) bib[x:y], x = from, y = to - 1)

  # Extract first line for speediness
  first_line <- sapply(item_list, function(x) x[1])

  keys <- gsub(".*((?<=\\{)[^,]+).*", "\\1", first_line, perl = TRUE)
  fields <- gsub(".*((?<=@)[^\\{]+).*", "\\1", first_line, perl = TRUE)
  fields <- tolower(fields)

  # TODO Implement checks for duplicate categories?
  categories <- get_bib_categories(item_list)
  values <- get_bib_values(item_list)

  bib_dataframe <- process_bib_dataframe(categories, values, fields, keys)
  bib_list <- process_bib_list(keys, fields, categories, values)

  as_bib(bib_dataframe, bib_list = bib_list)
}


# FUNS --------------------------------------------------------------------

get_bib_categories <- function(list) {
  lapply(list, function(x) {
    # Assuming categories are to the left of any "="
    xx <- strsplit(x[-1], "=")
    xx <- sapply(xx, `[`, 1)
    xx <- trimws(xx)
    xx[xx %in% c("{", "}")] <- NA_character_
    tolower(xx)
  })
}

get_bib_values <- function(list) {
  lapply(list, function(x) {
    # Assuming values are to the right of any "="
    xx <- strsplit(x[-1], "=")
    xx <- sapply(xx, `[`, -1)
    # remove empty columns
    xx[sapply(xx, function(x) length(x) == 0)] <- NA_character_
    # There may be something better than this
    # Would like to maintain the { and }
    # xx <- gsub("\\{|\\}|,?$", "", xx)
    xx <- trimws(xx)
    xx <- gsub("^(\\{|\")|(\"|\\})[,]?$", "", xx)
    xx
  })
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
#' @return A wide data.frame with explicit NAs

process_bib_dataframe <- function(categories, values, fields, keys) {
  # Determine all categories for missing values inside Map
  ucats <- unique_no_na(unlist(categories))
  ucats_n <- length(ucats)
  ucats_df <- structure(list(v1 = ucats, v2 = rep(NA_character_, ucats_n)),
                        class = "data.frame",
                        row.names = c(NA_integer_, ucats_n),
                        .Names = c("category", "value"))

  x <- Map(
    function(cats, vals, field, key) {
      # Determine valid categories/values
      valids <- !is.na(cats)
      cats <- cats[valids]
      vals <- vals[valids]

      # Check for duplicate categories
      lens <- tapply(cats, cats, length)
      bad <- lens > 1L

      if (any(bad)) {
        stop("The key `", key, "` has duplicate categories of `",
             names(lens)[bad], "`",
             call. = FALSE
        )
      }

      # Append vectors
      cats <- c("key", "field", cats)
      vals <- c(key, field, vals)

      # Create data.frame
      n <- length(vals)
      data <- structure(list(x1 = cats,
                             x2 = vals),
                        class = "data.frame",
                        row.names = c(NA_integer_, n),
                        .Names = c("category", "value"))

      # Check for missing categories
      toadd <- !ucats %in% cats
      if (any(toadd)) {
        data <- rbind(data, ucats_df[toadd, ])
      }

      # Transpose to prep for reduce rbinding
      new <- list2DF(as.list(data[[2]]))
      colnames(new) <- as.list(data[[1]])

      new
    },
    cats = categories,
    vals = values,
    field = fields,
    key = keys)

  Reduce(rbind, x)
}

process_bib_list <- function(keys, fields, categories, values) {
  x <- Map(
    function(key, field, cats, vals) {
      vals[is.na(vals)] <- ""
      new <- c(key, field, vals)
      names(new) <- c("key", "field", cats)
      class(new) <- c("character", "jordan_bib_entry", .Names = key)
      new
    },
    key = keys,
    field = fields,
    cats = categories,
    vals = values)
  as_bib_list(x, keys)
}

as_bib_list <- function(x, names = NULL) {
  stopifnot(is.list(x))
  structure(x, class = c("list", "jordan_bib_list"), names = names)
}

as_bib <- function(x, bib_list = NULL) {
  stopifnot(inherits(x, "data.frame"))
  structure(x, bib_list = bib_list, class = c("data.frame", "jordan_bib"))
}


# Prints ------------------------------------------------------------------

#' @export
#' @importFrom utils capture.output
print.jordan_bib_list <- function(x, ...) {
  nm <- names(x)
  out <- x
  names(out) <- NULL

  for (i in seq_along(out)) {
    nmi <- nm[i]
    bordern <- getOption("width") - nchar(nmi) - 1
    header <- if (bordern > 0) {
      paste0(" ", paste(rep("-", bordern), collapse = ""))
    } else {
      ""
    }

    co <- capture.output(print(out[[i]]))[-1]

    cat(paste0(ifelse(i == 1, "", "\n"), nmi, header),
        paste(paste("  ", co), collapse = "\n"),
        sep = "\n"
    )
  }
  invisible(x)
}


#' @export
print.jordan_bib_entry <- function(x, ...) {
  out <- x[!(is.na(x) | x == "")]
  nm <- names(out)
  chars <- nchar(nm)
  chars[is.na(chars)] <- 0L
  chars_max <- max(chars)

  blanks <- vapply(chars_max - chars + 2,
                   function(x) paste(rep(" ", x), collapse = ""),
                   character(1),
                   USE.NAMES = FALSE)
  cat(paste(nm, blanks, out), sep = "\n")
  invisible(x)
}
