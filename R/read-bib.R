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
#' @param n Integer, the maximum number of lines to read (passed to readLines)
#' @param encoding Assumed encoding of file (passed to readLines)
#'
#' @export
#'
#' @examples
#' file <- "https://raw.githubusercontent.com/jmbarbone/bib-references/master/references.bib"
#' bibdf <- read_bib(file, n = 51L)
#' if (requireNamespace("tibble")) tibble::as_tibble(bibdf) else head(bibdf)
#'
#' if (requireNamespace("bib2df") & requireNamespace("microbenchmark")) {
#'   file1 <- system.file("extdata", "bib2df_testfile_3.bib", package = "bib2df")
#'   microbenchmark::microbenchmark(
#'     read_bib(file1),
#'     bib2df::bib2df(file1)
#'   )
#' }

read_bib <- function(file, n = -1L, encoding = "UTF-8")
{
  bib <- readLines(file, n = n, encoding = encoding)
  bib <- gsub("[^[:graph:]]", " ", bib)

  # Find start of entries
  from <- grep("[@]", bib)
  stopifnot("No entries detected" = length(from) > 0)
  # shift over (may contain white space?)
  to <- c(from[-1] - 1, length(bib))

  itemlist <- mapply(function(x, y) bib[x:y],
                     x = from,
                     y = to - 1,
                     SIMPLIFY = FALSE)

  # Extract first line for speediness
  first_line <- sapply(itemlist, function(x) x[1])

  keys <- gsub(".*((?<=\\{)[^,]+).*", "\\1", first_line, perl = TRUE)

  fields <- gsub(".*((?<=@)[^\\{]+).*", "\\1", first_line, perl = TRUE)
  fields <- tolower(fields)

  # TODO Implement checks for duplicate categories?

  categories <- lapply(itemlist, function(x) {
    # Assuming categories are to the left of any "="
    xx <- strsplit(x[-1], "=")
    xx <- sapply(xx, `[`, 1)
    xx <- trimws(xx)
    xx[xx %in% c("{", "}")] <- NA_character_
    tolower(xx)
  })

  values <- lapply(itemlist, function(x) {
    # Assuming values are to the right of any "="
    xx <- strsplit(x[-1], "=")
    xx <- sapply(xx, `[`, -1)
    # remove empty columns
    xx[sapply(xx, function(x) length(x) == 0)] <- NA_character_
    xx <- gsub("\\{|\\}|,?$", "", xx)
    xx <- trimws(xx)
    xx
  })

  # Determine all categories for missing values inside Map
  ucats <- unique(unlist(categories))
  ucats <- ucats[!is.na(ucats)]
  ucats_n <- length(ucats)
  ucats_df <- structure(
    list(v1 = ucats,
         v2 = rep(NA_character_, ucats_n)),
    class = "data.frame",
    row.names = c(NA_integer_, ucats_n),
    .Names = c("category", "value"))

  items <- Map(
    function(cats, vals, field, key) {
      # Determine valids
      valids <- !is.na(cats)
      cats <- cats[valids]
      vals <- vals[valids]

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
    key = keys
  )


  bib_list <- Map(
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
    vals = values
  )

  bib_list <- structure(bib_list,
                        class = c("list", "jordan_bib_list"),
                        .Names = keys)

  structure(Reduce(rbind, items),
            bib_list = bib_list,
            class = c("data.frame", "jordan_bib")
  )
}

#' @export
print.jordan_bib_list <- function(x, ...) {
  # out <- sapply(x, function(x) capture.output(print(x)), USE.NAMES = FALSE)
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
  # x <- bib_list[[1]]
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
