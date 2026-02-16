# NA at positions

Converts select elements of a vector into `NA`s

This is how the end results are

- `NA_at` and `NA_if` require a suitable index value (`x[y] <- NA`)

  - `NA_at` expects `y` (or the result of function `y`) to be `integers`

  - `NA_if` expects `y` (or the result of function `y`) to be `logical`

- `NA_in` and `NA_out` expect some values to match on

  - `NA_in` checks `x[x %in% y] <- NA`

  - `NA_out` checks `x[x %out% y] <- NA` (see
    [fuj::match_ext](https://jmbarbone.github.io/fuj/reference/match_ext.html))

## Usage

``` r
NA_at(x, y, ...)

NA_if(x, y, ...)

NA_in(x, y, ...)

NA_out(x, y, ...)
```

## Arguments

- x:

  A vector of values

- y:

  Either a suitable value (see `Details`) or a function which accepts
  `x` as its first parameter and can return suitable values

- ...:

  Additional values passed to `y` (if `y` is a function)

## Value

`x` with assigned `NA` values

## Details

Convert specific values to NA

## See also

Inspired by
[`dplyr::na_if()`](https://dplyr.tidyverse.org/reference/na_if.html)

## Examples

``` r
let <- ordered(letters[1:5])
NA_at(let, c(1, 3, 5))   # [1] <NA> b    <NA> d    <NA>
#> [1] <NA> b    <NA> d    <NA>
#> Levels: a < b < c < d < e
NA_if(let, let <= "b")   # [1] <NA> <NA> c    d    e
#> [1] <NA> <NA> c    d    e   
#> Levels: a < b < c < d < e
NA_in(let, c("a", "c"))  # [1] <NA> b    <NA> d    e
#> [1] <NA> b    <NA> d    e   
#> Levels: a < b < c < d < e
NA_out(let, c("a", "c")) # [1] a    <NA> c    <NA> <NA>
#> [1] a    <NA> c    <NA> <NA>
#> Levels: a < b < c < d < e
```
