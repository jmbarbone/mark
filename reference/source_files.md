# Source file from directory

Walk through files in a directory and output them. Files are sources in
order of names

## Usage

``` r
source_r_dir(dir, echo = FALSE, quiet = FALSE, ...)

source_r_file(path, echo = FALSE, quiet = FALSE, ...)
```

## Arguments

- dir:

  The location of your R scripts

- echo:

  logical; if `TRUE`, each expression is printed after parsing, before
  evaluation.

- quiet:

  Logical. Whether to print out a message for each file.

- ...:

  Additional arguments passed to
  [`base::source()`](https://rdrr.io/r/base/source.html)

- path:

  The location of the R file.

## Value

None, called for side effects
