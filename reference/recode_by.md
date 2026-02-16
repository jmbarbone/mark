# Recode by

A simple implementation of recoding

## Usage

``` r
recode_by(x, by, vals = NULL, mode = "any")

recode_only(x, by, vals = NULL)
```

## Arguments

- x:

  A vector to recode

- by:

  A names vector (`new = old`); any non-matching values are set to the
  appropriate `NA`

- vals:

  An optional vector of values to use in lieu of a names in the vector;
  this takes priority over `names(by)`. This can be the same length as
  `by` or a single value.

- mode:

  passed to [`as.vector()`](https://rdrr.io/r/base/vector.html)

## Value

A vector of values from `x`

## Details

This can be comparable to
[`dplyr::recode()`](https://dplyr.tidyverse.org/reference/recode.html)
expect that the values are arranged as `new = old` rather than
`old = new` and allows for a separate vector to be passed for `new`.

`recode_only()` will only recode the values matches in `by`/`val`. The
`mode` is automatically set according to `mode(x)`. This functions more
like [`base::replace()`](https://rdrr.io/r/base/replace.html) but with
extra features

## See also

[`dplyr::recode()`](https://dplyr.tidyverse.org/reference/recode.html)

## Examples

``` r
recode_by(1:3, c(a = 1, b = 2))
#> [1] "a" "b" NA 
recode_by(letters[1:3], c(`1` = "a", `2` = "b"))                   # will not guess mode
#> [1] "1" "2" NA 
recode_by(letters[1:3], c(`1` = "a", `2` = "b"), mode = "integer") # make as integer
#> [1]  1  2 NA
recode_by(letters[1:3], c("a", "b"), vals = 1:2)                   # or pass to vals
#> [1]  1  2 NA

recode_only(letters[1:3], c("zzz" = "a"))
#> [1] "zzz" "b"   "c"  
recode_only(letters[1:3], c(`1` = "a")) # returns as "1"
#> [1] "1" "b" "c"
recode_only(1:3, c("a" = 1))            # coerced to NA
#> Warning: NAs introduced by coercion
#> [1] NA  2  3

# Pass list for multiples
recode_only(letters[1:10], list(abc = c("a", "b", "c"), ef = c("e", "f")))
#>  [1] "abc" "abc" "abc" "d"   "ef"  "ef"  "g"   "h"   "i"   "j"  
```
