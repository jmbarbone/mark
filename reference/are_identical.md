# Identical extensions

Extensions for the use of
[`base::identical()`](https://rdrr.io/r/base/identical.html)

## Usage

``` r
are_identical(..., params = NULL)
```

## Arguments

- ...:

  Vectors of values to compare, element-wise of equal length

- params:

  Additional params (as a named list of arguments for
  [base::identical](https://rdrr.io/r/base/identical.html))

## Value

A `logical` vector of `TRUE`/`FALSE` of equal length of each `...`
vector

## Examples

``` r
x <- y <- z <- 1:5
y[2] <- 3L
z[5] <- NA_integer_

identical(x, y)        # compare entire vector
#> [1] FALSE
are_identical(x, y)    # element-wise
#> [1]  TRUE FALSE  TRUE  TRUE  TRUE
are_identical(x, y, z) # 3 or more vectors
#> [1]  TRUE FALSE  TRUE  TRUE FALSE
```
