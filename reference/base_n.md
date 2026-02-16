# Base N conversion

Convert between base numbers

## Usage

``` r
base_n(x, from = 10, to = 10)
```

## Arguments

- x:

  A vector of integers

- from, to:

  An integer base to convert to and from; `from` must be an integer from
  `1` to `10` and `to` can currently only be `10`.

## Value

The A vector of integers converted from base `from` to base `to`

## Examples

``` r
base_n(c(24, 22, 16), from = 7)
#> [1] 18 16 13
```
