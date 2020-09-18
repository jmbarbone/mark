#' Start up functions
#'
#' Functions that try to assist with start ups
#'
#' @details
#' These functions can be used to create an environment for startup.
#'
#' @param env The environment to save objects
#' @name startup_funs
#' @family startup_utils
NULL


#' @export
#' @family startup_utils
#' @rdname startup_funs
.LoadFunctionsFromJordan <- function(env = parent.frame()) {
  e <- new.env()
  e$op <- options()
  j <- asNamespace("jordan")

  sf <- get("startup_funs", envir = j)
  for (f in sf) {
    fun <- get(f, envir = j)
    assign(f, fun, envir = env)
  }

  assign(".pe", e, envir = env)

  .ResetOptions <- function(keep_prompt = TRUE) {
    # Resets options
    # Calls the created .pe environment made in .LoadFunctionsFromJordan()
    # As far as I can tell, this has to be created inside the function to work
    #   properly.  I mean, I could always be wrong and not be good enough?
    op <- get("op", envir = get(".pe", env))
    if (keep_prompt) {
      op$prompt <- getOption("prompt")
    }
    on.exit(options(op), add = TRUE)
    inv()
  }

  assign(".ResetOptions", .ResetOptions, envir = env)

  inv()
}

#' @export
#' @rdname startup_funs
.RunDefaultFunctionsFromJordan <- function(env = parent.frame()) {
  eval({
    .NiceMessage()
    .load_pipe(env)
    .git_branch_prompt()
    # .ResetOptions()
  }, envir = env)

  inv()
}

#' @export
#' @rdname startup_funs
startup_funs <- c(
  '.RunDefaultFunctionsFromJordan',
  '.SendAttachedPackagesToREnviron',
  '.RemoveAttachedPackages',
  '.Reload',
  '.Restart',
  '.git_branch_prompt',
  '.NiceMessage',
  '.load_pipe',
  'ht',
  'ht.default',
  'ht.tbl_df',
  NULL
)


# Accessory ---------------------------------------------------------------



#' Manage attached packages
#'
#' Clears out attached packages and properly loads
#'
#' @param remove_renviron Logical, if TRUE will force an update to the
#'   .Renvironment file
#' @param attached A character vector of packages - if NULL will find packages
#'
#' @export
#' @family startup_funs
#' @name attached_packages
.SendAttachedPackagesToREnviron <- function(remove_renviron = FALSE) {
  attached <- grep("^package[:]", search(), value = TRUE)
  attached2 <- rev(gsub("^package[:]", "", attached))
  attached2 <- setdiff(attached2, .default_packages)

  file <- ".Renviron"

  if (isTRUE(remove_renviron)) {
    file.remove(file)
  }

  # If file exists, see if we need to remove the previous statement
  if (file.exists(file)) {
    rl <- readLines(".Renviron")
    line <- grep("jordan::.SendAttachedPackagesToREnviron()", rl, fixed = TRUE)

    if (length(line) == 1L) {
      cat(rl[-seq.int(line - 1L, line + 2L)], sep = "\n", file = file)
    }

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

  .RemoveAttachedPackages()
}


#' @export
#' @family startup_funs
#' @rdname attached_packages
.RemoveAttachedPackages <- function(attached = NULL) {
  if (is.null(attached)) {
    attached <- grep("^package[:]", search(), value = TRUE)
  } else {
    bad <- grep("^package[:]", attached, invert = TRUE)
    attached[bad] <- paste0("package:", attached[bad])
  }

  attached <- setdiff(attached, rev(names(.default_packages)))

  for (a in attached) {
    tryCatch(
      detach(a, character.only = TRUE),
      error = function(e) {
        warning("`", a, "` was not found `detach()`")
      },
      finally = function() {
        inv()
      })
  }

  inv()
}


.default_packages <- c("base", "datasets", "utils", "grDevices", "graphics", "stats", "methods")
names(.default_packages) <- paste0("package:", .default_packages)

#' Reload
#'
#' Reloads your session
#'
#' @param remove_objects Logical, if `TRUE` will remove all objects found
#' @param loud Logical, if `TRUE` will present a message on removed objects
#'
#' @export
#' @family startup_utils
#' @name Reload
.Reload <- function(remove_objects = TRUE, loud = FALSE) {
  cat(crayon::cyan("\nPreparing Restart ...\n"))

  objs <- ls(envir = .GlobalEnv, all.names = TRUE)

  if (remove_objects) {
    if (loud & length(objs) > 0L) {
      message("Removing all objects in the Global Environment:\n",
              paste(objs, collapse = " ... "))
    }
    rm(list = objs, envir = .GlobalEnv)
  }

  .Restart()
  inv()
}

#' @export
#' @name Reload
.Restart <- function() {
  rn_soft("rstudioapi")
  rstudioapi::restartSession()
}



#' Start up functions
#'
#' A collection of startup functions that require no parameters
#'
#' @rdname startup_funs
#' @family startup_utils
#' @export
.git_branch_prompt <- function() {
  rn_soft("prompt")
  branch <- prompt::prompt_git()

  if (branch != getOption("prompt", "> ") & branch != "> ") {
    branch_prompt <-  paste0("[", sub(" >", "] >", branch))
    prompt::set_prompt(branch_prompt)
  }
}


#' Adds a nice message to the start
#'
#' @param x A vector of integers to select from.  If the package isn't available
#'   another random message is generated -- or not
#'
#' @family startup_utils
#' @export
.NiceMessage <- function(x = 1:2) {
  if (length(x) == 0L) return(inv())
  x <- as.integer(x)

  switch(
    sample(x, 1),
    if (RN("praise")) cat_praise() else .NiceMessage(x[-1L]),
    if (RN("fortunes")) cat_fortune() else .NiceMessage(x[-2L])
  )

  inv()
}

cat_praise <- function() {
  cat("\n", crayon::yellow(praise::praise()), "\n\n")
  inv()
}

cat_fortune <- function() {
  x <- paste(capture.output(fortunes::fortune()))
  cat(crayon::yellow(x), sep = "\n")
  inv()
}

#' @rdname startup_funs
#' @export
#' @family startup_utils
.load_pipe <- function(env = parent.frame()) {
  if (!"%>%" %in% ls(envir = env)) {
    assign("%>%", magrittr::`%>%`, envir = env)
  }

  inv()
}


#' Head-Tail
#'
#' Quick head & tail function
#'
#' @details
#' This contains a separate method for class `tbl_df`
#'
#' @param x A data.frame
#' @param n The number of rows to see (if length 1L, is repeated for head and
#'   tail)
#'
#' @importFrom utils head
#' @importFrom utils tail
#'
#' @family startup_utils
#'
#' @importFrom utils head
#' @importFrom utils tail
#'
#' @export
ht <- function(x, n = 5L) {
  UseMethod("ht", x)
}

#' @export
ht.default <- function(x, n = 5L) {
  stopifnot(is.data.frame(x) | is.matrix(x))

  if (length(n) == 1L) n[2] <- n

  cat(capture.output(head(x, n[1])), sep = "\n")
  cat("\t...\t...\t... \n")
  # rownames don't line up
  cat(capture.output(tail(x, n[2]))[-1], sep = "\n")

  invisible(x)
}

#' @export
ht.tbl_df <- function(x, n = 5L) {
  if (length(n) == 1L) n[2] <- n

  cat(capture.output(head(x, n[1]))[-1], sep = "\n")
  cat("\t...\t...\t... \n")
  # rownames don't line up
  # need to do this to maintain row numbers
  xx <- as.data.frame(x)
  cat(capture.output(tail(xx, n[2]))[-1], sep = "\n")

  invisible(x)
}


# Environment -------------------------------------------------------------

.startup_env_file <- function() {
  x <- file_path(Sys.getenv("R_USER"), "startup_files/jordan_startup_env.rds")
  dir.create(dirname(x), recursive = TRUE, showWarnings = FALSE)
  x
}

.set_startup_env <- function() {
  e <- new.env(parent = baseenv())
  e$op <- options()
  f <- .startup_env_file()
  if (file.exists(f)) file.remove(f)
  con <- file(f)
  suppressWarnings(saveRDS(e, file = con))

  on.exit(close(con))
  invisible(e)
}

.remove_startup_env <- function() {
  op <- .getstartup_env()$op

  on.exit(options(op))
  file.remove(.startup_env_file())
  inv()
}

.getstartup_env <- function() {
  readRDS(file = .startup_env_file())
}


# e <- .set_startup_env()
# e
# .getstartup_env()
# options(not.real = TRUE)
# .remove_startup_env()
# getOption("not.real")


# FUNS --------------------------------------------------------------------


remove_tag_and_save <- function(file, tag) {
  x <- readLines(file)
  x <- paste(x, collapse = "\n")
  pattern <- wrap_tags(tag, ".*", sep = "", add_returns = FALSE)
  out <- gsub(pattern, "", x)
  out <- gsub("(\n)+$", "", out)
  writeLines(out, file)
}

wrap_tags <- function(tag, ..., sep = "\n", add_returns = FALSE) {
  ls <- list(...)
  stopifnot(length(ls) > 0L)
  ls <- unlist(ls)
  tags <- paste0(sprintf("# @madrigal_%s_", tag), c("start", "stop"))

  if (add_returns) {
    tags[1] <- paste0("\n", tags[1])
    tags[2] <- paste0(tags[2], "\n")
  }

  x <- paste(ls, collapse = "")
  paste(list(tags[1], x, tags[2]), collapse = sep)
}
