# Alpha base

Base 26 conversion with letters

## Usage

``` r
base_alpha(x, base = 26)
```

## Arguments

- x:

  A string of letters. Non characters are removed.

- base:

  A numeric

## Value

A vector of integers

## Examples

``` r
base_alpha("AB")
#> [1] 28
base_alpha("XFD")
#> [1] 4684
base_alpha(c("JMB", "Jordan Mark", "XKCD"))
#> Warning: NAs introduced by coercion to integer range
#> [1]  9050    NA 60792
sum(base_alpha(c("x", "k", "c", "d")))
#> [1] 42
```
