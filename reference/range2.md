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

  A numeric (or character) vector (see Note in
  [base::min](https://rdrr.io/r/base/Extremes.html))

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
#>   0.023   0.006   0.028 
system.time(rep(range2(x), 100))
#>    user  system elapsed 
#>   0.015   0.000   0.014 
x[sample(x, 1e5)] <- NA

system.time(rep(range(x, na.rm = TRUE), 100))
#>    user  system elapsed 
#>   0.356   0.014   0.370 
system.time(rep(range2(x, na.rm = TRUE), 100))
#>    user  system elapsed 
#>   0.015   0.000   0.015 
# }
```
