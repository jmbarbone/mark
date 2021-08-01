if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

if (file.exists("~/.Rprofile")) {
  source("~/.Rprofile", print.eval = TRUE)
}

# Don't need anymore
Sys.setenv(TESTTHAT_CPUS = 3)
options(Ncpus = 3)
