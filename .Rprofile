
# profile environment
.pe <- new.env()
.pe$op <- options()

if (requireNamespace("prompt", quietly = TRUE)) {
  .pe$branch <- prompt::prompt_git()
  .pe$branch_prompt <-  paste0("[", sub(" >", "] >", .pe$branch))
  prompt::set_prompt(.pe$branch_prompt)
}

.Restart <- rstudioapi::restartSession

.Reload <- function(remove_objects = TRUE, loud = FALSE) {
  .pe$lss <- ls(envir = .GlobalEnv)

  if (remove_objects) {
    if (loud & length(.pe$lss) > 0L) {
      message("Removing all objects in the Global Environment:\n",
              paste(.pe$lss, collapse = " ... "))
    }
    rm(list = .pe$lss, envir = .GlobalEnv)
  }

  .Restart()
  invisible(NULL)
}

.ResetOptions <- function() {
  options(.pe$op)
  invisible(NULL)
}

# Option preferences
options(tidyverse.quiet = TRUE)

# Load in the pipe because I'm lazy
`%>%` <- magrittr::`%>%`

# Head tail function
ht <- function(x, n = 5L) {
  stopifnot(is.data.frame(x))

  if (length(n) == 1L) n[2] <- n

  cat(capture.output(head(x, n[1])), sep = "\n")
  cat("\t...\t...\t... \n")
  # rownames don't line up
  cat(capture.output(tail(x, n[2]))[-1], sep = "\n")
  invisible(x)
}

# Be nice to yourself
switch(
  sample(1:2, 1),
  if (requireNamespace("praise", quietly = TRUE)) cat(praise::praise(), "\n"),
  if (requireNamespace("fortunes", quietly = TRUE)) fortunes::fortune()
)

# Would these make sense anymore?
# R_PROFILE_USER --> /.Rprofile --> ~/.Rprofile
#   Would effectively check that .Rprofile does not equal ~/.Profile?
.rprofile <- function() {
  tryCatch({
    normalizePath("~/.rprofile",
                  winslash = .Platform$file.sep,
                  mustWork = TRUE)
  },
  error = function(e){
    stop(".Rprofile not found in: ", path.expand("~"), call. = FALSE)
  })
}

.RProfileUserCheck <- function() {
  rprof <- .rprofile()

  # Maybe add in check for path.expand("~/.Rprofile)
  stopifnot("R_PROFILE_USER has been set; won't compare" = Sys.getenv("R_PROFILE_USER") == "")

  if (requireNamespace("waldo", quietly = TRUE)) {
    diffs <- waldo::compare(readLines(rprof), readLines(".Rprofile"),
                            x_arg = "~/.Rprofile",
                            y_arg = ".Rprofile")

    if (length(diffs)) {
      cat(diffs)
      switch(
        utils::menu(title = "\n\nWould you like to update the file?",
                    choices = c("No",
                                "Yes -- Update .Rprofile",
                                "Yes -- Updated ~/.Rprofile")),
        `1` = cat("Okay, just checking"),
        `2` = {
          file.copy(rprof, ".Rprofile", overwrite = TRUE, copy.date = TRUE)
          .Reload()
        },
        `3` = {
          file.copy(".Rprofile", rprof, overwrite = TRUE, copy.date = TRUE)
          .Reload()
        })
    } else {
      cat("No differences found")
    }
  }
}
