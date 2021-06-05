library(mark, warn.conflicts = FALSE)
library(bench)
library(bib2df)

file0 <- "https://raw.githubusercontent.com/jmbarbone/bib-references/master/references.bib"
file <- system.file("extdata", "bib2df_testfile_3.bib", package = "bib2df")

# Doesn't include the 'tidying' up
foo <- function(file) {
  bib <- ("bib2df" %colons% "bib2df_read")(file)
  ("bib2df" %colons% "bib2df_gather")(bib)
}

bench::mark(
  `read_bib` = mark::read_bib(file),
  `bib2df` = bib2df::bib2df(file),
  `foo` = foo(file),

  `read_bib` = mark::read_bib(file0),
  `bib2df` = bib2df::bib2df(file0),
  `foo` = foo(file0),

  iterations = 20,
  check = FALSE
)

# bench:::autoplot.bench_mark(.Last.value)
