# Unique rows

Drops duplicated rows

## Usage

``` r
unique_rows(data, cols = NULL, from_last = FALSE, invert = FALSE)
```

## Arguments

- data:

  A `data.frame`

- cols:

  Columns to compare against; when `NULL` selects all columns

- from_last:

  When `TRUE` returns the last row containing duplicates, rather than
  the first

- invert:

  If `TRUE` returns the duplicated rows

## Value

`data` will duplicates removes

## Examples

``` r
df <- quick_dfl(
  i = 1:4,
  a = rep(1:2, 2L),
  b = rep("a", 4L),
)

unique_rows(df, 2:3)
#>   i a b
#> 1 1 1 a
#> 2 2 2 a
unique_rows(df, c("a", "b"), from_last = TRUE, invert = TRUE)
#>   i a b
#> 1 1 1 a
#> 2 2 2 a
```
