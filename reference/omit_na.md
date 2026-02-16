# Omit NA values

Omit NA values

## Usage

``` r
omit_na(x)
```

## Arguments

- x:

  A vector of values

## Value

`x` which `NA` values removes and two attributes of `integers`: `na`
which is the position of `NA` values, and `valid` for the position of
non-`NA` values; empty positions reported as `integer(0)`

## Examples

``` r
# Like stats::na.omit but always provides
x <- letters[1:5]
omit_na(x)
#> [1] "a" "b" "c" "d" "e"
#> attr(,"na")
#> integer(0)
#> attr(,"valid")
#> [1] 1 2 3 4 5
x[c(3, 5)] <- NA
omit_na(x)
#> [1] "a" "b" NA  "d" NA 
#> attr(,"na")
#> [1] 3 5
#> attr(,"valid")
#> [1] 1 2 4
```
