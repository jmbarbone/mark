# Wildcard globbing

Helper function for globbing character vectors

## Usage

``` r
glob(x, pattern = NULL, value = TRUE, ...)
```

## Arguments

- x:

  A vector of characters

- pattern:

  Wildcard globbing pattern

- value, ...:

  Additional parameters passed to `grep`. Note: `value` is by default
  `TRUE`; when `NA`, `...` is passed to `grepl`.

## Examples

``` r
x <- c("apple", "banana", "peach", "pear", "orange")
glob(x, "*e")
#> [1] "apple"  "orange"
glob(x, "pea*", value = FALSE)
#> [1] 3 4
glob(x, "*an*", value = NA)
#> [1] FALSE  TRUE FALSE FALSE  TRUE

path <- system.file("R", package = "mark")
glob(list.files(path), "r*")
#> character(0)
```
