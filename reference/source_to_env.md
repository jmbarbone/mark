# Source to environment

Source an R script to an environment

## Usage

``` r
source_to_env(x, ops = NULL)
```

## Arguments

- x:

  An R script

- ops:

  Options to be passed to
  [rscript](https://jmbarbone.github.io/mark/reference/rscript.md)

## Value

Invisibly, and environment variable of the objects/results created from
`x`
