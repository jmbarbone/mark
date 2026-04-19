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
x <- c("apple", "banana", "orange")
suppressWarnings(match_arg("b", x), "deprecatedWarning")
#> Warning: <deprecated_warning>
#> mark::match_arg() is deprecated, use mark::match_param() instead and will be removed in 0.10.0
#> [1] "banana"

# Produces error
suppressWarnings(try(match_arg("pear", x)), "deprecatedWarning")
#> Warning: <deprecated_warning>
#> mark::match_arg() is deprecated, use mark::match_param() instead and will be removed in 0.10.0
#> Error in match_arg() : <mark:match_arg_error>
#> pear : 'pear' did not match of of the following:
#>    'apple'
#> pear : 'pear' did not match of of the following:
#>    'banana'
#> pear : 'pear' did not match of of the following:
#>    'orange'

foo <- function(x, op = c(1, 2, 3)) {
  op <- suppressWarnings(match_arg(op), "deprecatedWarning")
  x / op
}

foo(10, 3)
#> Warning: <deprecated_warning>
#> mark::match_arg() is deprecated, use mark::match_param() instead and will be removed in 0.10.0
#> [1] 3.333333

# Error
try(foo(1, 0))
#> Warning: <deprecated_warning>
#> mark::match_arg() is deprecated, use mark::match_param() instead and will be removed in 0.10.0
#> Error in match_arg() : <mark:match_arg_error>
#> op : '0' did not match of of the following:
#>    '1'
#> op : '0' did not match of of the following:
#>    '2'
#> op : '0' did not match of of the following:
#>    '3'
```
