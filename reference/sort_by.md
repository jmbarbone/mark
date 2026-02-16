# Sort by

Sort an object by another object

## Usage

``` r
sort_by(x, by, ...)
```

## Arguments

- x:

  A vector

- by:

  Another vector

- ...:

  Additional arguments passed to
  [`base::order()`](https://rdrr.io/r/base/order.html)

## Value

The values of `x`, resorted

## Examples

``` r
l3 <- letters[1:3]
sort_by(l3, c(3, 2, 1))
#> [1] "c" "b" "a"
# make a factor object with the reversed order
f <- factor(l3, levels = rev(l3))
sort_by(f, l3)
#> [1] a b c
#> Levels: c b a
sort_by(1:3, rev(l3))
#> [1] 3 2 1
```
