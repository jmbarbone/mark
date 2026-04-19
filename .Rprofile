if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

# Some issue where this was failing in tests
Sys.setenv(TESTTHAT_PARALLEL = if (getRversion() < "4") "FALSE" else "TRUE")

if (file.exists("~/.Rprofile")) {
  source("~/.Rprofile", print.eval = TRUE)
}

cat("\n")
