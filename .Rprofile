if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

if (file.exists("~/.Rprofile")) {
  source("~/.Rprofile", print.eval = TRUE)
}

# Don't need anymore
Sys.setenv(TESTTHAT_CPUS = 3)
options(Ncpus = 3)

# Some issue where this was failing in tests
Sys.setenv(TESTTHAT_PARALLEL = if (getRversion() < 4) "FALSE" else "TRUE")
