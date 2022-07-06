#' Time reports
#'
#' Evaluate code and report on the time difference
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function can be used to evaluate an expression line-by-line to capture
#'   outputs, errors, messages, and evaluation time.
#'
#' @param title The title to be printed
#' @param expr The expression to run
#' @param envir The environment from which to evaluate the `expr`
#' @return A `reported_results`/`list` object containing results, outputs,
#'   messages, warnings, and errors
#'
#' @examples
#' simpleTimeReport("example", {
#'   print("1")
#'   Sys.sleep(1)
#'   warning("this is a warning")
#'   for (i in 1:5) {
#'     Sys.sleep(0.5)
#'   }
#'   sample(1e6, 1e6, TRUE)
#' })
#' @export

simpleTimeReport <- function(title = NULL, expr, envir = parent.frame()) {
  mc <- match.call()
  cat0(trimws(title), "\n", rep("-", getOption("width")), "\n")
  line <- rep("-", getOption("width"))

  .start_time <- Sys.time()
  expr <- as.expression(as.vector(sys.call(), "character")[3])
  exprs <- split_expression(expr)

  results <- messages <- warnings <- list()

  calls <- as.vector(exprs, "character")
  calls <- gsub("\n", "", calls)
  calls <- gsub("[[:space:]]+", " ", calls)
  n <- length(calls)
  outputs <- rep_len(list(), n)

  lens <- nchar(calls)
  len_add <- max(lens) - lens + 2L
  ne <- new.env()

  local({
    for (i in seq_along(calls)) {
      .st <- Sys.time()
      cat0(calls[i], rep(" ", len_add[i]))
      withCallingHandlers(
        # capture any outputs separately than the results
        outputs[[i]] <<-
          utils::capture.output(results[[i]] <<-
              eval(exprs[i], envir)),
        error = function(e)
          stop("\n", e$message, call. = FALSE),
        warning = function(e) {
          warnings[[i]] <<- e$message
          # Only invoke Restart if it is the same warning?
          invokeRestart("muffleWarning")
        },
        message = function(e) {
          messages[[i]] <<- e$message
          invokeRestart("muffleMessage")
        })
      cat0(crayon_cyan(formatTimeDiff(.st)), "\n")
    }
  }, ne)

  cat0(line, "\n")

  cat0("Finished ", crayon_cyan(formatTimeDiff(.start_time)), "\n")

  # Maybe add these to the print method?
  if (!identical(warnings, list()) && any(!vap_lgl(warnings, is.null))) {
    cat0("\nWarnings\n", line, "\n")
    for (i in seq_along(warnings)) {
      if (!is.null(warnings[[i]])) {
        catln(sprintf("%i : %s", i, calls[i]))
        warning(simpleWarning(warnings[[i]], calls[i]))
      }
    }
  }

  if (!identical(messages, list()) && any(!vap_lgl(messages, is.null))) {
    cat0("\nMessages\n", line, "\n")
    for (i in seq_along(messages)) {
      if (!is.null(messages[[i]])) {
        catln(sprintf("%i : %s", i, calls[i]))
        message(sprintf("Message in : %s\n%s", messages[[i]], calls[i]))
      }
    }
    catln(calls[i])
  }

  if (!identical(outputs, rep_len(list(), n)) && any(!vap_lgl(outputs, is.null))) {
    cat0("\nOutputs\n", line, "\n")
    for (i in seq_along(outputs)) {
      if (!is.null(outputs[[i]]) && !identical(outputs[[i]], character())) {
        catln(sprintf("%i : %s", i, calls[i]))
        catln(outputs[[i]])
      }
    }
  }

  res <- struct(
    results,
    class    = c("reported_results", "list"),
    names    = calls,
    call     = mc,
    messages = messages,
    warnings = warnings,
    outputs  = outputs
  )

  invisible(res)
}

#' Format time difference
#'
#' Provides quick formatting for time differences object
#'
#' @description
#' A preferred method of printing out a time differences, assuming it is to be
#'   appended to some other message
#'
#' @param start A start time
#' @param stop A stop time (default to `Sys.time()`)
#' @param threshold A threshold for reporting the difference, if `stop - start`
#'   is less than this, a empty character vector (`""`) is returned
#' @noRd
formatTimeDiff <- function(start, stop = Sys.time(), threshold = .1) {
  difference <- stop - start

  threshold <- as.difftime(threshold, units = "secs")
  if (difference < threshold) {
    return("")
  }

  u <- substr(attr(difference, "units"), 1L, 1L)
  sprintf("[%.2f %s]", difference, u)
}

#' Split an expression
#'
#' Split an expression into multiple expressions
#'
#' @description
#' Can be useful when passing an expression with curly braces to separate into
#'   separate expressions to evaluate (and report) out one by one
#'
#' @param expr An expression to split
#' @examples
#' ss <- mark:::split_expression({
#'   mean(1:10)
#'   sample(
#'     letters,
#'     5,
#'     TRUE
#'   )
#'   two <- 2
#'   two * 2
#' })
#'
#' for (i in ss) {
#'   str(mark:::charexpr(i))
#' }
#' @noRd
split_expression <- function(expr) {
  if (!is.expression(expr)) {
    expr <- as.expression(substitute(expr))
  }
  x <- as.character(expr)
  x <- as.vector(parse(text = x, -1L), "character")
  x <- trimws(x, "left", "{\n?\\s{0,}")
  x <- trimws(x, "right", "\\s{0,}\n?}")
  x <- trimws(x)
  x <- strsplit(x, "\\s{0,}\n\\s{0,}")[[1]]
  str2expression(x)
}


