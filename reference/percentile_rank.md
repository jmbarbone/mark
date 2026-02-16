# Percentile rank

The bounds of the percentile rank are \> 0 and \< 1 (see Boundaries)

A percentile rank here is the proportion of scores that are less than
the current score.

\$\$PR = (c_L + 0.5 f_i) / N\$\$

Where

\\c_L\\ is the frequency of scores less than the score of interest

\\f_i\\ is the frequency of the score of interest

## Usage

``` r
percentile_rank(x, weights = times, times)
```

## Arguments

- x:

  A vector of values to rank

- weights, times:

  A vector of the number of times to repeat `x`

## Value

The percentile rank of `x` between 0 and 1 (see Boundaries)

## Details

Computes a percentile rank for each score in a set.

## Boundaries

While the percentile rank of a score in a set must be exclusively within
the boundaries of `0` and `1`, this function may produce a percentile
rank that is exactly `0` or `1`. This may occur when the number of
values are so large that the value within the boundaries is too small to
be differentiated.

Additionally, when using the `weights` parameter, if the lowest or
highest number has a value of `0`, the number will then have a
theoretical `0` or `1`, as these values are not actually within the set.

## Examples

``` r
percentile_rank(0:9)
#>    0    1    2    3    4    5    6    7    8    9 
#> 0.05 0.15 0.25 0.35 0.45 0.55 0.65 0.75 0.85 0.95 
x <- c(1, 2, 1, 7, 5, NA_integer_, 7, 10)
percentile_rank(x)
#>         1         2         1         7         5      <NA>         7        10 
#> 0.1428571 0.3571429 0.1428571 0.7142857 0.5000000        NA 0.7142857 0.9285714 

if (package_available("dplyr")) {
  dplyr::percent_rank(x)
}
#> [1] 0.0000000 0.3333333 0.0000000 0.6666667 0.5000000        NA 0.6666667
#> [8] 1.0000000

# with times
percentile_rank(7:1, c(1, 0, 2, 2, 3, 1, 1))
#>    7    6    5    4    3    2    1 
#> 0.95 0.90 0.80 0.60 0.35 0.15 0.05 
```
