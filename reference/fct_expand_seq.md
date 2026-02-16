# Factor Expand by Sequence

Expands an ordered factor from one level to another

## Usage

``` r
fct_expand_seq(
  x,
  min_lvl = min(x, na.rm = TRUE),
  max_lvl = max(x, na.rm = TRUE),
  by = 1L
)
```

## Arguments

- x:

  An ordered factor

- min_lvl:

  The start of the level sequence

- max_lvl:

  The end of the level sequence

- by:

  Integer, number of steps in between

## Value

An `ordered` vector

## Details

Defaults for `min_lvl` and `max_lvl` are the minimum and maximum levels
in the ordered vector `x`.

## Examples

``` r
x <- ordered(letters[c(5:15, 2)], levels = letters)
fct_expand_seq(x)
#>  [1] b c d e f g h i j k l m n o
#> 26 Levels: a < b < c < d < e < f < g < h < i < j < k < l < m < n < o < ... < z
fct_expand_seq(x, "g", "s", 3L) # from "g" to "s" by 3
#> [1] g j m p s
#> 26 Levels: a < b < c < d < e < f < g < h < i < j < k < l < m < n < o < ... < z
fct_expand_seq(x, "g", "t", 3L) # same as above
#> [1] g j m p s
#> 26 Levels: a < b < c < d < e < f < g < h < i < j < k < l < m < n < o < ... < z

# from the first inherit level to the last observed
fct_expand_seq(x, min(levels(x)))
#>  [1] a b c d e f g h i j k l m n o
#> 26 Levels: a < b < c < d < e < f < g < h < i < j < k < l < m < n < o < ... < z
```
