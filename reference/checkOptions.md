# Check options

For each name in `x` checks the current option value and reports if
there is a difference in a `message`. This does not change the options

## Usage

``` r
checkOptions(x)
```

## Arguments

- x:

  A named list of new options

## Value

Invisible, a list of the current options from
[`options()`](https://rdrr.io/r/base/options.html)

## Details

Checks and reports on options

## Examples

``` r
op <- options()

x <- list(width = -20, warning.length = 2, probably_not_a_real_option = 2)
checkOptions(x)
#> Option(s) updated :
#>  "width"
#>    old : 80
#>    new : -20
#>  "warning.length"
#>    old : 1000
#>    new : 2
# pointless, but shows that no messages are given
identical(options(), checkOptions(options()))
#> [1] TRUE

options(op)
```
