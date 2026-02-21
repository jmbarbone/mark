# Details an object

Provides details about an object

## Usage

``` r
detail(x, ...)

# Default S3 method
detail(x, factor_n = 5L, ...)

# S3 method for class 'data.frame'
detail(x, factor_n = 5L, ...)
```

## Arguments

- x:

  An object

- ...:

  Additional arguments passed to methods

- factor_n:

  An `integer` threshold for making factors; will convert any character
  vectors with `factor_n` or less unique values into a `fact`; setting
  as `NA` will ignore this

## Examples

``` r
x <- sample(letters[1:4], 10, TRUE)
detail(x)
#>          class    type label  n na min_c max_c level level_n note comment
#> 1 fact; factor integer  <NA> 10  0     1     1     a       3 <NA>    <NA>
#> 2 fact; factor integer  <NA> 10  0     1     1     b       3 <NA>    <NA>
#> 3 fact; factor integer  <NA> 10  0     1     1     c       1 <NA>    <NA>
#> 4 fact; factor integer  <NA> 10  0     1     1     d       3 <NA>    <NA>

df <- quick_df(list(
  x = x,
  y = round(runif(10), 2),
  z = Sys.Date() + runif(10) * 100
))

detail(df)
#>   i col        class    type label  n na      min_c      max_c level level_n
#> 1 1   x fact; factor integer  <NA> 10  0          1          1     a       3
#> 2 1   x fact; factor integer  <NA> 10  0          1          1     b       3
#> 3 1   x fact; factor integer  <NA> 10  0          1          1     c       1
#> 4 1   x fact; factor integer  <NA> 10  0          1          1     d       3
#> 5 2   y      numeric  double  <NA> 10  0       0.07       0.83  <NA>      NA
#> 6 3   z         Date  double  <NA> 10  0 2026-02-23 2026-05-26  <NA>      NA
#>   note comment
#> 1 <NA>    <NA>
#> 2 <NA>    <NA>
#> 3 <NA>    <NA>
#> 4 <NA>    <NA>
#> 5 <NA>    <NA>
#> 6 <NA>    <NA>
```
