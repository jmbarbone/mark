# Ordered

As ordered

## Usage

``` r
as_ordered(x)

# Default S3 method
as_ordered(x)
```

## Arguments

- x:

  A vector of values

## Value

An `ordered` vector

## Details

Simple implementation of `ordered`. If `x` is `ordered` it is simply
returned. If `x` is a `factor` the `ordered` class is added. Otherwise,
`x` is made into a `factor` with
[`fact()`](https://jmbarbone.github.io/mark/reference/fact.md) and then
the `ordered` class is added. Unlike just `fact`, `ordered` will replace
the `NA` levels with `NA_integer_` to work appropriately with other
functions.

## See also

[`fact()`](https://jmbarbone.github.io/mark/reference/fact.md)

Other factors:
[`char2fact()`](https://jmbarbone.github.io/mark/reference/char2fact.md),
[`drop_levels()`](https://jmbarbone.github.io/mark/reference/drop_levels.md),
[`fact()`](https://jmbarbone.github.io/mark/reference/fact.md),
[`fact2char()`](https://jmbarbone.github.io/mark/reference/fact2char.md),
[`fact_na()`](https://jmbarbone.github.io/mark/reference/fact_na.md)

## Examples

``` r
x <- c("a", NA, "b")
x <- fact(x)
str(x) # NA is 3L
#>  Factor w/ 3 levels "a","b",NA: 1 3 2
#>  - attr(*, "uniques")= chr [1:3] "a" "b" NA
#>  - attr(*, "na")= int 3

y <- x
class(y) <- c("ordered", class(y))
max(y)
#> [1] <NA>
#> Levels: a < b
max(y, na.rm = TRUE) # returns NA -- bad
#> [1] b
#> Levels: a < b

# as_ordered() removes the NA level
x <- as_ordered(x)
str(x)
#>  Ord.factor w/ 2 levels "a"<"b": 1 NA 2
#>  - attr(*, "uniques")= chr [1:2] "a" "b"
#>  - attr(*, "na")= int 0
max(x, na.rm = TRUE) # returns b -- correct
#> [1] b
#> Levels: a < b
```
