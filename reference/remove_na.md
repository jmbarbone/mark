# Remove NA

Remove NAs from a vector

## Usage

``` r
remove_na(x)

# Default S3 method
remove_na(x)

# S3 method for class 'list'
remove_na(x)

# S3 method for class 'factor'
remove_na(x)

# S3 method for class 'fact'
remove_na(x)
```

## Arguments

- x:

  A vector of values

## Value

`x` without values where `is.na(x)` is `TRUE` For factors, a new factor
(`ordered` if `is.ordered(x)`)

## Details

`remove_na.factor` will remove `NA` values as identified by the
[`levels()`](https://rdrr.io/r/base/levels.html) or by the integer value
of the level. `factors` are recreated with all `NA` values and, if
present, the `NA` `level` removed.

## Examples

``` r
remove_na(c(4, 1, 2, NA, 4, NA, 3, 2))
#> [1] 4 1 2 4 3 2

# removes based on levels
remove_na(fact(c("b", NA, "a", "c")))
#> [1] b a c
#> Levels: b a c

# removes based on values
x <- as_ordered(c("b", "d", "a", "c"))
x[2:3] <- NA
str(remove_na(x))
#>  Ord.factor w/ 4 levels "b"<"d"<"a"<"c": 1 4
#>  - attr(*, "uniques")= chr [1:4] "b" "d" "a" "c"
#>  - attr(*, "na")= int 0
```
