# Match arguments

This function is essentially a clear version of
[`base::match.arg()`](https://rdrr.io/r/base/match.arg.html) which
produces a cleaner warning message and does not restrict the `table`
param to `character` vectors only.

## Usage

``` r
match_arg(x, table)
```

## Arguments

- x:

  An argument

- table:

  A table of choices

## Value

A single value from `x` matched on `table`

## Details

Match arguments

## See also

[`match_param()`](https://jmbarbone.github.io/mark/reference/match_param.md)

## Examples

``` r
shh <- function(expr) suppressWarnings(expr, "deprecated_warning")
x <- c("apple", "banana", "orange")
shh(match_arg("b", x))
#> [1] "banana"

# Produces error
shh(try(match_arg("pear", x)))
#> Error in match_arg() : <mark::match_arg_error>
#> pear : 'pear' did not match of of the following:
#>    'apple'
#> pear : 'pear' did not match of of the following:
#>    'banana'
#> pear : 'pear' did not match of of the following:
#>    'orange'

foo <- function(x, op = c(1, 2, 3)) {
  op <- shh(match_arg(op))
  x / op
}

foo(10, 3)
#> [1] 3.333333

# Error
try(foo(1, 0))
#> Error in match_arg() : <mark::match_arg_error>
#> op : '0' did not match of of the following:
#>    '1'
#> op : '0' did not match of of the following:
#>    '2'
#> op : '0' did not match of of the following:
#>    '3'
```
