#' Start up functions
#'
#' Functions that try to assit with start ups


#' Manage attached packages
#'
#' Clears out attached packages and properly loads
#'
#' @param remove_renviron Logical, if TRUE will force an update to the
#'   .Renvironment file
#'
#' @export
#' @name attached_packages
.SendAttachedPackagesToREnviron <- function(remove_renviron = FALSE) {
  attached <- grep("^package[:]", search(), value = TRUE)
  attached2 <- rev(gsub("^package[:]", "", attached))
  attached2 <- setdiff(attached2, .default_packages)

  file <- ".Renviron"
  file_remove_renviorn <- file.exists(file) & remove_renviron

  if (file_remove_renviorn) {
    file.remove(file)
  }

  # If file exists, this appends to the file
  cat(
    "\n# Below created with ",
    "jordan::.SendAttachedPackagesToREnviron() ",
    "from your .Rprofile\n",
    "R_DEFAULT_PACKAGES='",
    paste(c(.default_packages, attached2), collapse = ","),
    "'\n",
    sep = "",
    file = file,
    append = !file_remove_renviorn
  )

  # Remove `base` which is always last
  .RemoveAttachedPackages(attached2)
}

#' @export
#' @rdname attached_packages
.RemoveAttachedPackages <- function(attached = NULL) {
  if (is.null(attached)) {
    attached <- grep("^package[:]", search(), value = TRUE)
  }

  attached <- setdiff(attached, rev(.default_packages))

  for (a in attached) {
    detach(a, character.only = TRUE)
  }

  invisible(NULL)
}


.default_packages <- c("base", "datasets", "utils", "grDevices", "graphics", "stats", "methods")
names(.default_packages) <- paste0("package:", .default_packages)

#' @export
.Reload <- function (remove_objects = TRUE, loud = FALSE) {
  objs <- ls(envir = .GlobalEnv, all.names = TRUE)
  if (remove_objects) {
    if (loud & length(objs) > 0L) {
      message("Removing all objects in the Global Environment:\n",
              paste(objs, collapse = " ... "))
    }
    rm(objs, envir = .GlobalEnv)
  }
  .Restart()
  invisible(NULL)
}

#' @export
.Restart <- rstudioapi::restartSession

#' Update your git branch
#'
#' Mostly a wrapper for prompt::git() but with brackets
#'
#' @export
.git_branch <- function() {
  branch <- prompt::prompt_git()
  branch_prompt <-  paste0("[", sub(" >", "] >", branch))
  prompt::set_prompt(branch_prompt)
}


#' Adds a nice message to the start
#'
#' @param x A vector of integers to select from.  If the package isn't available
#'   another random message is generated -- or not
#' @export
.NiceMessage <- function(x = 1:2) {
  x <- as.integer(x)
  RN <- function(x) requireNamespace(x, quietly = TRUE)

  switch(
    sample(x, 1),
    if (RN("praise")) cat(praise::praise(), "\n") else .NiceMessage(x[-1L]),
    if (RN("fortunes")) fortunes::fortune() else .NiceMessage(x[-2L])
  )
}

#' @export
.load_pipe <- function() {
  if (!"%>%" %in% ls(envir = .GlobalEnv)) {
    assign("%>%", `magrittr::`%>%`, envir = .GlobalEnv)
  }

  invisible()
}

#' Head-Tail
#'
#' Quick head & tail function
#'
#' @details
#' This contains a separate method for class `tbl_df`
#'
#' @param x A data.frame
#' @param n The number of rows to see (if length 1 is repeated for head and tail)
#' @export
ht <- function(x, n = 5L) {
  UseMethod("ht", x)
}

ht.default <- function(x, n = 5L) {
  stopifnot(is.data.frame(x))

  if (length(n) == 1L) n[2] <- n

  cat(capture.output(head(x, n[1])), sep = "\n")
  cat("\t...\t...\t... \n")
  # rownames don't line up
  cat(capture.output(tail(x, n[2]))[-1], sep = "\n")

  invisible(x)
}

ht.tbl_df <- function(x, n = 5L) {
  if (length(n) == 1L) n[2] <- n

  cat(capture.output(head(x, n[1]))[-1], sep = "\n")
  cat("\t...\t...\t... \n")
  nr <- nrow(x)
  # rownames don't line up
  # need to do this to maintain row numbers
  xx <- as.data.frame(x)
  cat(capture.output(tail(xx, n[2]))[-1], sep = "\n")

  invisible(x)
}

.LoadFunctionsFromJordan <- function() {
  # List functions with the tag
  # Assign functions
  invisible(NULL)
}
