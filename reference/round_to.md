# Round to

Rounds a vector to the nearest in a set of anchors.

## Usage

``` r
round_to(x, anchors)
```

## Arguments

- x:

  A vector of values

- anchors:

  A vector of anchor values

## Examples

``` r
x <- rpois(10, 1)
anchors <- c(0, 0.5, 1.5, 3)
data.frame(x, anchor = round_to(x, anchors))
#>    x anchor
#> 1  0    0.0
#> 2  0    0.0
#> 3  1    0.5
#> 4  1    0.5
#> 5  0    0.0
#> 6  1    0.5
#> 7  3    3.0
#> 8  1    0.5
#> 9  2    1.5
#> 10 2    1.5
```
