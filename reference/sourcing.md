# Sourcing extensions

Functions for extending sourcing features

## Usage

``` r
ksource(file, ..., quiet = TRUE, cd = FALSE, env = parent.frame())

try_source(file, cd = FALSE, ...)

try_ksource(file, ...)
```

## Arguments

- file:

  An R or Rmd file.

- ...:

  Additional arguments passed to
  [`base::source()`](https://rdrr.io/r/base/source.html)

- quiet:

  Logical; Determines whether to apply silence to
  [`knitr::purl()`](https://rdrr.io/pkg/knitr/man/knit.html)

- cd:

  Logical; if TRUE, the R working directory is temporarily changed to
  the directory containing file for evaluating

- env:

  An environment determining where the parsed expressions are evaluated

## Value

- `ksource()`: Invisibly, the result of calling
  [`source()`](https://rdrr.io/r/base/source.html) on the `.R` file
  conversion of `file`

- `try_source()`, `try_ksource()`: attempts of
  [`source()`](https://rdrr.io/r/base/source.html) and `ksource()` but
  converts errors to warnings

## Details

`try_source()` will output an error message rather than completely
preventing the execution. This can be useful for when a script calls on
multiple, independent files to be sourced and a single failure shouldn't
prevent the entire run to fail as well.
