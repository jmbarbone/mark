#' Engine - Rust
#'
#' Set Rust engine for knitr
#'
#' @details
#' If dependencies are needed they should be defined in a `cargo.toml` file in
#'   the working directory and the code chunk option `toml=TRUE` should be set.
#'
#' Additional options:
#' \describe{
#'   \item{timeout}{Time (in seconds) to wait for program to finish}
#'   \item{env}{Environment to execute code}
#' }
#'
#' @param options Options passed to code chunk
#' @references Adapted from
#'   https://greenwood.dev/2019/12/22/running-rust-in-rmd/
#' @export
engine_rust <- function(options) {
  if (isFALSE(options$eval)) {
    return()
  }

  code <- options$code
  cmd0 <- file.path(Sys.getenv("USERPROFILE"), ".cargo", "bin")
  toml <- isTRUE(options$toml)
  env <- options$engine.env
  env <- env %||% character()
  remove_src <- FALSE
  remove_temp_files <- FALSE
  timeout <- options$timeout %||% 0L
  wd <- getwd()
  setwd(tempdir())

  if (toml) {
    stopifnot(file.copy(file.path(wd, "cargo.toml"), "cargo.toml"))
    cmd <- paste0(cmd0, "/cargo")
    file <- "src/main.rs"

    # May not be a problem if all in tempdir?
    stopifnot("src/main.rs already exists" = !file.exists(file))

    if (!dir.exists("src")) {
      remove_src <- TRUE
      dir.create("src")
    }

  } else {
    cmd <- paste0(cmd0, "/rustc")
    remove_temp_files <- TRUE
    file <- basename(tempfile(fileext = ".rs"))
    rustc_args <- paste0(" ", file)
  }

  # Write file and prepare to remove
  xfun::write_utf8(code, file)

  on.exit({
    # Reset wd
    setwd(wd)

    # May not be necessary if all done in temp files?
    if (remove_src) {
      unlink("src", recursive = TRUE)
    }

    if (remove_temp_files) {
      files <- tools::file_path_sans_ext(basename(file))
      files <- paste0(files, c(".exe", ".rs", ".pdb"))
      invisible(file.remove(files[file.exists(files)]))
    }

    if (file.exists("Cargo.lock")) {
      file.remove("Cargo.lock")
    }
  },
  add = TRUE)

  if (!toml) {
    message("Running: ", basename(cmd), rustc_args)
    tryCatch(
      system0(cmd, args = rustc_args, env = env, timeout = timeout),
      error = function(e) {
        if (!isTRUE(options$error)) stop(e)
      }
    )
  }

  extra <- if (toml) {
    args <- paste0(" run --target-dir ", tempdir())
    message("Running: ", basename(cmd), args)

    tryCatch(
      system0(cmd, args = args, env = env, timeout = timeout),
      error = function(e) {
        e <- paste0("Error in running Rust code\n", e)
        if (isFALSE(options$error)) {
          stop(e, .call = FALSE)
        }
        simpleWarning(e)
      }
    )
  } else {
    run_exe <- paste0("./", sub(".rs", "", file))
    message('Running: ', paste0(basename(run_exe), ".exe"))

    tryCatch(
      system0(run_exe, env = env, timeout = timeout),
      error = function(e) {
        if (isFALSE(options$error)) stop(e)
        'Error in executing rust code'
      }
    )
  }

  if (options$results == 'hide') {
    extra <- NULL
  }

  knitr::engine_output(options, code, extra)
}

#' @rdname engine_rust
#' @export
set_rust_engine <- function() {
  # Add checks for paths?
  bin <- file.path(Sys.getenv("USERPROFILE"), ".cargo", "bin")
  stopifnot(file.exists(file.path(bin, "cargo.exe")),
            file.exists(file.path(bin, "rustc.exe")))

  knitr::knit_engines$set(rust = engine_rust)
}

# shorthand for some preferences
system0 <- function(cmd, args = character(), env = character(), timeout = 0) {
  if (length(args) && grepl("^\\s", args)) {
    args <- paste0(" ", args)
  }

  system2(
    cmd,
    args   = args,
    stdout = TRUE,
    stderr = TRUE,
    env    = env,
    wait   = FALSE,
    timeout = timeout
  )
}