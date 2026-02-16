# Depth

Functions to extract the 'depth' of an object

## Usage

``` r
depth(x, ...)

# Default S3 method
depth(x, ...)

# S3 method for class 'list'
depth(x, ...)
```

## Arguments

- x:

  An object

- ...:

  Possible additional arguments passed to methods (not in use)

## Value

A single `integer`

## Details

This function does not count an empty lists
([`list()`](https://rdrr.io/r/base/list.html)) as a level or `NULL`
objects.

## Examples

``` r
a <- c(1, 2, 3)
depth(a) # Vectors are 1L
#> [1] 1

b <- list(a = 1, b = list(list(1)))
depth(b)
#> [1] 3
```
