if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

# Don't need anymore
Sys.setenv(TESTTHAT_CPUS = 4)
options(Ncpus = 4)

# Some issue where this was failing in tests
Sys.setenv(TESTTHAT_PARALLEL = if (getRversion() < 4) "FALSE" else "TRUE")

if (file.exists("~/.Rprofile")) {
  source("~/.Rprofile", print.eval = TRUE)
}

cat("\n")
try(dang::checkCRANStatus("jmbarbone@gmail.com"))
