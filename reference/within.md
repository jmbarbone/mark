# within boundaries

Compare a vector within (between) other values

## Usage

``` r
between_more(x, left, right, type = c("gele", "gel", "gle", "gl"))

within(x, left = NULL, right = NULL, bounds = c("[]", "[)", "(]", "()"))
```

## Arguments

- x:

  A numeric vector of values

- left, right:

  Boundary values. For `within()`, when `NULL` no comparison is made for
  that boundary. When both are `NULL`, `x` is just returned.

- type:

  Abbreviation for the evaluation of `left` on `right` (see details)

- bounds:

  Boundaries for comparisons of `left` and `right` (see details)

## Value

A logical vector

## Details

``` type``,  ```bounds“ can be one of the below:

- g,(:

  is greater than (\>)

- ge,\[:

  greater than or equal to (\>=)

- l,)):

  less than (\<)

- le,\[\]:

  less than or equal to (\<=)

Note: `between_more()` may be deprecated in the future in favor of just
`within()`

## Examples

``` r
between_more(2:10, 2, 10, "gl")
#> [1] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
within(2:10, 2, bounds = "()")
#> [1] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
between_more(10, 2, 10, "gle")
#> [1] TRUE
within(2:10, bounds = "(]")
#> [1]  2  3  4  5  6  7  8  9 10
within(1:5, c(3, 3, 2, 2, 1), 5)
#> [1] FALSE FALSE  TRUE  TRUE  TRUE
```
