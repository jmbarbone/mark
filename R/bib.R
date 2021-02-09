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
#' @seealso `bib2df::bib2df()`
#'
#' @export
#'
#' @examples
#' file <- "https://raw.githubusercontent.com/jmbarbone/bib-references/master/references.bib"
#' bibdf <- read_bib(file, max_lines = 51L)
#'
#' \dontrun{
#' if (rn("tibble")) {
#'     tibble::as_tibble(bibdf)
#'   } else {
#'     head(bibdf)
#'   }
#'
#' if (rn("bib2df") & rn("microbenchmark")) {
#'   file <- system.file("extdata", "bib2df_testfile_3.bib", package = "bib2df")
#'
#'   # Doesn't include the 'tidying' up
#'   foo <- function(file) {
#'     bib <- ("bib2df" %colons% "bib2df_read")(file)
#'     ("bib2df" %colons% "bib2df_gather")(bib)
#'   }
#'
#'   bench::mark(
#'     `read_bib` = read_bib(file1),
#'     `bib2df` = bib2df::bib2df(file1),
#'     `foo` = foo(file1),
#'     check = FALSEread
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

  if (length(from) == 0L) {
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
  categories <- get_bib_categories(item_list)
  values <- get_bib_values(item_list)

  as_bib(
    process_bib_dataframe(categories, values, fields, keys),
    bib_list = process_bib_list(keys, fields, categories, values)
  )
}


# FUNS --------------------------------------------------------------------

get_bib_categories <- function(list) {
  lapply(list, function(x) {
    # Assuming categories are to the left of any "="
    xx <- strsplit(x[-1L], "=")
    xx <- sapply(xx, `[`, 1L)
    xx <- trimws(xx)
    xx[xx %in% c("{", "}")] <- NA_character_
    tolower(xx)
  })
}

get_bib_values <- function(list) {
  lapply(list, function(x) {
    # Assuming values are to the right of any "="
    xx <- strsplit(x[-1L], "=")
    xx <- sapply(xx, `[`, -1L)
    # remove empty columns
    xx[lengths(xx) == 0L] <- NA_character_
    # There may be something better than this
    # Would like to maintain the { and }
    # xx <- gsub("\\{|\\}|,?$", "", xx)
    xx <- trimws(xx)
    xx <- gsub("^(\\{|\")|(\"|\\})[,]?$", "", xx)
    xx <- gsub(",$", "", xx)
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
  ucats <- unique(remove_na(unlist(categories)))
  ucats_df <- quick_df(list(category = ucats, value = rep(NA_character_, length(ucats))))

  x <- Map(
    function(cats, vals, field, key) {
      # Determine valid categories/values
      valids <- !is.na(cats)
      cats <- cats[valids]
      vals <- vals[valids]

      # Check for duplicate categories
      lens <- lengths(split(cats, cats))
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
      data <- quick_df(list(category = cats, value = vals))

      # Check for missing categories
      toadd <- ucats %out% cats
      if (any(toadd)) {
        data <- rbind(data, ucats_df[toadd, ])
      }

      # Transpose to prep for reduce rbinding
      new <- list2df2(as.list(data[[2L]]))
      colnames(new) <- as.list(data[[1L]])

      new
    },
    cats = categories,
    vals = values,
    field = fields,
    key = keys
  )

  Reduce(rbind, x)
}

process_bib_list <- function(keys, fields, categories, values) {
  valid <- !is.na(values) & values != ""

  x <- Map(
    function(key, field, cats, vals) {
      # vals[is.na(vals)] <- ""
      new <- c(key, field, vals)
      names(new) <- c("key", "field", cats)
      class(new) <- c("character", "jordan_bib_entry", .Names = key)
      new
    },
    key = keys[valid],
    field = fields[valid],
    cats = categories[valid],
    vals = values[valid]
  )

  as_bib_list(x, keys[valid])
}

as_bib_list <- function(x, names = NULL) {
  if (!is.list(x)) {
    stop("`x` must be a list", call. = FALSE)
  }
  attr(x, "class") <- c("list", "jordan_bib_list")
  x
}

as_bib <- function(x, bib_list = NULL) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data.frame", call. = FALSE)
  }

  attr(x, "class") <- c("data.frame", "jordan_bib")
  x
}


# Prints ------------------------------------------------------------------

#' @export
print.jordan_bib_list <- function(x, ...) {
  nm <- names(x)
  out <- x
  names(out) <- NULL

  for (i in seq_along(out)) {
    nmi <- nm[i]
    bordern <- getOption("width") - nchar(nmi) - 1L
    header <- if (bordern > 0L) {
      paste0(" ", collapse0(rep("-", bordern), sep = ""))
    } else {
      ""
    }

    co <- utils::capture.output(print(out[[i]]))[-1L]

    cat(paste0(ifelse(i == 1L, "", "\n"), nmi, header),
        collapse0(paste0("  ", co), sep = "\n"),
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

  blanks <- vap_chr(
    chars_max - chars + 2L,
    function(x) {
      collapse0(rep(" ", x), sep = "")
    }
  )

  cat(paste(nm, blanks, out), sep = "\n")
  invisible(x)
}
