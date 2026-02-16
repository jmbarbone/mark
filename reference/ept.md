# Parse and evaluate text

A wrapper for eval(parse(text = .))

## Usage

``` r
ept(x, envir = parent.frame())
```

## Arguments

- x:

  A character string to parse

- envir:

  The environment in which to evaluate the code

## Value

The evaluation of `x` after parsing
