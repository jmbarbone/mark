# nolint start: line_length_linter.

#' Sourcing extensions
#'
#' Functions for extending sourcing features
#'
#' @param file An R or Rmd file.
#' @param quiet Logical; Determines whether to apply silence to [knitr::purl()]
#' @param cd Logical; if TRUE, the R working directory is temporarily
#'   changed to the directory containing file for evaluating
#' @param env An environment determining where the parsed expressions are
#'   evaluated
#' @param ... Additional arguments passed to [base::source()]
#'
#' @details
#' `try_source()` will output an error message rather than completely preventing
#'   the execution.
#' This can be useful for when a script calls on multiple, independent files to
#'   be sourced and a single failure shouldn't prevent the entire run to fail as
#'   well.
#'
#' @name sourcing
#' @return
#' * `ksource()`: Invisibly, the result of calling `source()` on the `.R` file conversion of `file`
#' * `try_source()`, `try_ksource()`: attempts of `source()` and `ksource()` but converts errors to warnings
#' @export

# nolint end: line_length_linter.

ksource <- function(file, ..., quiet = TRUE, cd = FALSE, env = parent.frame()) {
  require_namespace("knitr")
  stopifnot(is.environment(env))
  o <- mark_temp("R")
  on.exit(file.remove(o), add = TRUE)
  source(knitr::purl(file, output = o, quiet = quiet), chdir = cd, local = env)
}

#' @rdname sourcing
#' @export
try_source <- function(file, cd = FALSE, ...) {
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
try_ksource <- function(file, ...) {
  tryCatch(
    ksource(file = file, ...),
    error = function(e) {
      warning(e, call. = FALSE)
    },
    simpleWarning = function(e) {
      warning(e, call. = FALSE)
    })
}

# nolint start: commented_code_linter.

#' Evaluate a  Named Chunk
#'
#' Evaluate a named chunk from an Rmd file.
#'
#' @param rmd_file Absolute path to rmd file
#' @param label_name Name of label
#' @return The value from the evaluated code chunk
#'
#' @export
#'
#' @examples
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
#' print(TRUE)
#' ```
#'
#' ```{r another label}
#' warning("wrong label")
#' ```
#' '
#' \dontrun{
#' writeLines(text, con = temp_rmd)
#'
#' eval_named_chunk(temp_rmd, "hello label")
#' # [1] "hello, world"
#' # [1] TRUE
#'
#' file.remove(temp_rmd)
#' }

# nolint end: commented_code_linter.

eval_named_chunk <- function(rmd_file, label_name) {
  if (!grepl("\\.[Rr][Mm][Dd]$", rmd_file)) {
    stop(cond_eval_named_chunk_rmd())
  }

  lines <- readLines(rmd_file)
  label_line <- grep(paste0("\\{r ", label_name), lines)[1]

  if (is.na(label_line)) {
    stop(cond_eval_named_chunk_label())
  }

  lines <- lines[(label_line + 1):length(lines)]
  exp <- lines[1:(grep("[```]", lines)[1] - 1)]
  ept(exp, envir = new.env())
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
#' @return None, called for side effects
#'
#' @export
#' @name source_files

source_r_dir <- function(dir, echo = FALSE, quiet = FALSE, ...) {
  files <- list.files(dir, pattern = "\\.[rR]$", full.names = TRUE)
  invisible(lapply(sort(files), source_r_file, q = quiet, ...))
}

#' @export
#' @rdname source_files
#' @inheritParams source_files
source_r_file <- function(path, echo = FALSE, quiet = FALSE, ...) {
  if (!grepl("\\.[rR]$", path)) {
    stop(cond_source_r_file_r())
  }

  stopifnot(is_file(path))

  st <- system.time(
    tryCatch(
      source(path, echo = echo, ..., chdir = FALSE),
      error = function(e) {
        stop("Error in ", path, "\n", e, call. = FALSE)
      }
    )
  )

  if (!quiet) {
    message(
      sprintf("Successfully sourced: %s [%s]",
        basename(path),
        round(st[["elapsed"]], 2)
      )
    )
  }

  invisible()
}

# Rscript -----------------------------------------------------------------

# Functions for "safe" sourcing, which can be used to launch a stand-alone
#   script which may require the R objects created to be reused.
#

#' Source to environment
#'
#' Source an R script to an environment
#'
#' @param x An R script
#' @param ops Options to be passed to [mark::rscript]
#' @return Invisibly, and environment variable of the objects/results created
#'   from `x`
#' @export
source_to_env <- function(x, ops = NULL) {
  rds_file <- mark_temp("Rds")
  r_temp   <- mark_temp("R")
  std_out  <- mark_temp("md")
  std_err  <- mark_temp("md")

  file.copy(x, r_temp)

  line_end <- sprintf(
    '\nmark::save_source(file = "%s", name = "%s")\n',
    rds_file,
    basename(x)
  )

  cat(line_end, sep = "", file = r_temp, append = TRUE)
  rscript(r_temp, ops, wait = TRUE, stdout = std_out, stderr = std_err)

  if (!is_file(rds_file)) {
    stop(cond_source_to_env_fail(
      file = rds_file,
      err = std_err,
      out = std_out
    ))
  }

  con <- file(rds_file)
  res <- readRDS(con)

  on.exit({
    close(con)
    file.remove(r_temp, rds_file, std_out, std_err)
  }, add = TRUE)

  invisible(res)
}

#' Rscript
#'
#' Implements `Rscript` with `system2`
#'
#' @param x An R file to run
#' @param ops A character vector of options (`"--"` is added to each)
#' @param args A character vector of other arguments to pass
#' @param ... Additional arguments passed to `system2`
#' @return A `character` vector of the result from calling `Rscript` via
#'   `system2()`
#'
#' @seealso [mark::source_to_env]
#' @export
rscript <- function(x, ops = NULL, args = NULL, ...) {
  if (length(ops) > 0) {
    ops <- paste0("--", ops)
  }

  rs <- file_path(
    R.home("bin"), if (is_windows()) "Rscript.exe" else "Rscript",
    check = TRUE
  )

  x <- norm_path(x, check = TRUE)
  system2(command = rs, args = c(ops, shQuote(x), args), ...)
}

#' Save source
#'
#' Source a file and save as file
#'
#' @param env The parent environment
#' @param file The file to save the environment to
#' @param name An optional name for the environment (mostly cosmetic)
#' @return A `source_env`/`environment` object, created from `env`
#'
#' @export
save_source <- function(
    env = parent.frame(),
    file = mark_temp("Rds"),
    name = NULL
) {
  ls <- ls(envir = env, all.name = TRUE)
  out <- lapply(ls, get, envir = env)
  names(out) <- ls

  res <- struct(
    list2env(out, parent = baseenv()),
    class       = c("source_env", "environment"),
    sessionInfo = utils::sessionInfo(),
    search      = search(),
    options     = options(),
    file        = file,
    env         = env,
    name        = name %||% file_name(file)
  )

  if (!is.null(file)) {

    if (!is_dir(dirname(file))) {
      dir.create(dirname(file), recursive = TRUE)
    }

    con <- file(file)
    on.exit(close(con), add = TRUE)
    saveRDS(res, file = con, version = 2)
  }

  res
}

#' @export
print.source_env <- function(x, ...) {
  a <- attributes(x)
  cat(
    "<", crayon_green("sourced env: "), a$name, ">\n",
    "<", crayon_green("parent env: "), environmentName(a$env), ">\n",
    sep = ""
  )
  invisible(x)
}

# conditions --------------------------------------------------------------

cond_eval_named_chunk_rmd <- function() {
  new_condition(
    "rmd_file does not appear to be an rmd file",
    "eval_named_chunk_rmd"
  )
}

cond_eval_named_chunk_label <- function() {
  new_condition("label not found in .Rmd file", "eval_named_chunk_label")
}

cond_source_r_file_r <- function() {
  new_condition("Must be a .R file", "source_r_file_r")
}

cond_source_to_env_fail <- function(file, err, out) {
  msg <- paste0(
    "RDS file not succesfully saved here:\n  ", file,
    "\n",
    "\nRscript stderr:\n",
    collapse0(readLines(err), sep = "\n"),
    "\n",
    "\nRscript stdout:\n",
    collapse0(readLines(out), sep = "\n"),
    "\n"
  )
  new_condition(msg, "source_to_env_fail")
}

# globalVariables ---------------------------------------------------------

utils::globalVariables(c("source_file_r", "quiet"))
