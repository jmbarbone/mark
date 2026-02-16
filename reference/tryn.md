# Try an expression a set number of times

Try an expression a set number of times

## Usage

``` r
tryn(expr, n = 10, silent = TRUE)
```

## Arguments

- expr:

  expression to evaluate

- n:

  number of attempts until error

- silent:

  whether to suppress warnings

## Value

result of `expr`

## Examples

``` r
foo <- function() stop("I added an error")
try(tryn(n = 10, foo()))
#> Error : tryn() failed: maximum attempts reached: 10
#> Error in foo() : I added an error
```
