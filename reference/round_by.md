# Rounding by a specific interval.

Rounds a number or vector of numbers by another

## Usage

``` r
round_by(x, by = 1, method = c("round", "ceiling", "floor"), include0 = TRUE)
```

## Arguments

- x:

  A number or vector to round.

- by:

  The number by which to round

- method:

  An option to explicitly specify automatic rounding, ceiling, or floor

- include0:

  If `FALSE` replaces `0` with `by`

## Value

A vector of `doubles` of the same length of `x`

## Examples

``` r
x <- seq(1, 13, by = 4/3)

cbind(
  x,
  by_1 = round_by(x, 1),
  by_2 = round_by(x, 2),
  by_3 = round_by(x, 3)
)
#>               x by_1 by_2 by_3
#>  [1,]  1.000000    1    0    0
#>  [2,]  2.333333    2    2    3
#>  [3,]  3.666667    4    4    3
#>  [4,]  5.000000    5    4    6
#>  [5,]  6.333333    6    6    6
#>  [6,]  7.666667    8    8    9
#>  [7,]  9.000000    9    8    9
#>  [8,] 10.333333   10   10    9
#>  [9,] 11.666667   12   12   12
#> [10,] 13.000000   13   12   12
```
