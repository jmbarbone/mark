
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

  if (remove_objects && length(.pe$lss) > 0L && loud) {
    message("Removing all objects in the Global Environment:\n",
            paste(.pe$lss, collapse = " ... "))
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
if(stats::runif(1) > .5) {
  if (requireNamespace("praise", quietly = TRUE)) cat(praise::praise(), "\n")
} else {
  if (requireNamespace("fortunes", quietly = TRUE)) fortunes::fortune()
}

# This is maybe dangerous but it is package development, right?"

.RProfileUserCheck <- function() {
  rpu <- Sys.getenv("R_PROFILE_USER")
  if (rpu == "") return(invisible(NULL))
  if (requireNamespace("waldo", quietly = TRUE)) {
    diffs <- waldo::compare(readLines(rpu), readLines(".Rprofile"))
    if (length(diffs)) {
      cat(diffs, "\n")
      switch(
        utils::menu(title = "Differences foundnWould you like to update the file?",
                    choices = c("No",
                                "Yes -- Update .Rprofile",
                                "Yes -- Updated R_PROFILE_USER")),
        `2` = cat("Okay, just checking"),
        `2` = {
          file.copy(rpu, ".Rprofile", overwrite = TRUE, copy.date = TRUE)
          .Reload()
        },
        `3` = {
          file.copy(".Rprofile", rpu, overwrite = TRUE, copy.date = TRUE)
          .Reload()
        })
     } else {
        cat("No differences found")
      }
  }
}

