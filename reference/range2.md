# Range 2

Employs [`base::min()`](https://rdrr.io/r/base/Extremes.html) and
[`base::max()`](https://rdrr.io/r/base/Extremes.html). However,
[`base::range()`](https://rdrr.io/r/base/range.html), there is no
argument for removing `Inf` values.

## Usage

``` r
range2(x, na.rm = FALSE)
```

## Arguments

- x:

  A numeric (or character) vector (see Note in base::min)

- na.rm:

  Logical, if `TRUE` removes missing values

## Value

A `numeric` vector of length 2 of the minimum and maximum values,
respectively

## Examples

``` r
# \donttest{
x <- rep(1:1e5, 100)
system.time(rep(range(x),  100))
#>    user  system elapsed 
#>   0.025   0.003   0.028 
system.time(rep(range2(x), 100))
#>    user  system elapsed 
#>   0.014   0.000   0.014 
x[sample(x, 1e5)] <- NA

system.time(rep(range(x, na.rm = TRUE), 100))
#>    user  system elapsed 
#>   0.385   0.018   0.403 
system.time(rep(range2(x, na.rm = TRUE), 100))
#>    user  system elapsed 
#>   0.015   0.000   0.014 
# }
```
