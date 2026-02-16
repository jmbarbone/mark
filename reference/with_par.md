# Temporary plotting

Reset par() after running

## Usage

``` r
with_par(..., ops = NULL)
```

## Arguments

- ...:

  Code to be evaluated

- ops:

  A named list to be passed to
  [`graphics::par()`](https://rdrr.io/r/graphics/par.html)

## Value

Invisibly, the result of `...`

## Examples

``` r
with_par(
  plot(lm(Sepal.Length ~ Sepal.Width, data = iris)),
  plot(lm(Petal.Length ~ Petal.Width, data = iris)),
  ops = list(mfrow = c(2, 4))
)
```
