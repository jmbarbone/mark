#' Sourcing extensions
#'
#' Functions for extending sourcing features
#'
#' @param file An R or Rmd file.
#' @param ... Additional arguments, see details.
#' @param quiet Logical; Determines whether to apply silence to [knitr::purl()]
#' @param cd Logical; if TRUE, the R working directory is temporarily
#'   changed to the directory containing file for evaluating
#'
#' @details
#'
#' `try_source()` will output an error message rather than completely preventing the execution.
#' This can be useful for when a script calls on multiple, independent files to be sourced
#' and a single failure shouldn't prevent the entire run to fail as well.
#'
#' `ksource()` ... sends additional arguments to [knitr::purl()]
#'
#' @name sourcing
#' @export

ksource <- function(file, ..., quiet = TRUE, cd = FALSE)
{
  require_namespace("knitr")
  source(
    knitr::purl(
      file,
      output = tempfile(),
      quiet = quiet,
      ...
    ),
    chdir = cd
  )
}


#' @rdname sourcing
#' @export
try_source <- function(file, ..., cd = FALSE) {
  tryCatch(
    source(file, chdir = cd),
    error = function(e) {
      warning(e, call. = FALSE)
    },
    simpleWarning = function(e) {
      warning(e, call. = FALSE)
    })
}

#' @rdname sourcing
#' @export
try_ksource <- function(file, ..., cd = FALSE) {
  tryCatch(
    ksource(file = file, ..., cd = cd),
    error = function(e) {
      warning(e, call. = FALSE)
    },
    simpleWarning = function(e) {
      warning(e, call. = FALSE)
    })
}


#' Evaluate a  Named Chunk
#'
#' Evaluate a named chunk from an Rmd file.
#'
#' @param rmd_file Absolute path to rmd file
#' @param label_name Name of label
#' @param ... Additional arguments passed to [base::eval()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' temp_rmd <- tempfile(fileext = ".rmd")
#'
#' text <- '
#' ```{r not this label}
#' print("that is wrong")
#' ```
#'
#' ```{r hello label}
#' text <- "hello, world"
#' print(text)
#' ```
#'
#' ```{r another label}
#' warning("wrong label")
#' ```
#' '
#'
#' writeLines(text, con = temp_rmd)
#'
#' read_named_chunk(temp_rmd, "hello label")
#' }

eval_named_chunk <- function(rmd_file, label_name, ...) {
  stopifnot(tolower(tools::file_ext(rmd_file)) == "rmd")

  lines <- readLines(rmd_file)
  label_line <- grep(paste0("\\{r ", label_name), lines)[1]

  stopifnot(!is.na(label_line))

  lines <- lines[(label_line + 1):length(lines)]
  exp <- lines[1:(grep("[```]", lines)[1] - 1)]
  eval(parse(text = exp), ...)
}


#' Source file from directory
#'
#' Walk through files in a directory and output them.
#' Files are sources in order of names
#'
#' @param dir The location of your R scripts
#' @param quiet Logical.  Whether to print out a message for each file.
#' @param path The location of the R file.
#' @inheritParams base::source
#' @param ... Additional arguments passed to [base::source()]
#'
#' @export
#' @name source_files

source_r_dir <- function(dir, echo = FALSE, quiet = FALSE, ...) {
  files <- list.files(dir, pattern = "\\.[rR]$", full.names = TRUE)
  invisible(lapply(sort(files), source_r_file, q = quiet, ...))
}

#' @export %>%
#' @rdname source_files
source_dir_r <- function(dir, echo = FALSE, quiet = FALSE, ...) {
  warning("Use `jordan::source_r_dir()` instead.",  call. = FALSE)
  source_r_dir(dir, echo, quiet, ...)
}

#' @export
#' @rdname source_files
#' @inheritParams source_files
source_r_file <- function(path, echo = FALSE, quiet = FALSE, ...) {
  stopifnot("Must be a .R file" = grepl("\\.[rR]$", path))

  if (!file.exists(path)) {
    stop(sprintf("File << %s >> not found.", path), call. = FALSE)
  }

  st <- system.time(
    tryCatch(
      source(path, echo = echo, ..., chdir = FALSE),
      error = function(e) stop("Error in ", path, "\n", e, call. = FALSE)
    )
  )

  if (!quiet) {
    message(sprintf("Successfully sourced: %s [%s]",
                    basename(path),
                    round(st[["elapsed"]], 2)))
  }
  invisible()
}

utils::globalVariables(c("source_file_r", "quiet"))
