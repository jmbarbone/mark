# Rscript

Implements `Rscript` with
[`base::system2()`](https://rdrr.io/r/base/system2.html)

## Usage

``` r
rscript(x, ops = NULL, args = NULL, ...)
```

## Arguments

- x:

  An R file to run

- ops:

  A character vector of options (`"--"` is added to each)

- args:

  A character vector of other arguments to pass

- ...:

  Additional arguments passed to
  [`base::system2()`](https://rdrr.io/r/base/system2.html)

## Value

A `character` vector of the result from calling `Rscript` via
[`system2()`](https://rdrr.io/r/base/system2.html)

## See also

[source_to_env](https://jmbarbone.github.io/mark/reference/source_to_env.md)
