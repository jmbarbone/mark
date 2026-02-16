# Complete cases

Return completed cases of a data.frame

## Usage

``` r
complete_cases(data, cols = NULL, invert = FALSE)
```

## Arguments

- data:

  A data.frame

- cols:

  Colnames or numbers to remove `NA` values from; `NULL` (default) will
  use all columns

- invert:

  Logical, if `TRUE` will return incomplete cases

## Value

A `data.frame`

## Examples

``` r
x <- data.frame(
  a = 1:5,
  b = c(1, NA, 3, 4, 5),
  c = c(1, NA, NA, 4, 5)
)

complete_cases(x)
#>   a b c
#> 1 1 1 1
#> 2 4 4 4
#> 3 5 5 5
complete_cases(x, invert = TRUE) # returns the incomplete rows
#>   a  b  c
#> 1 2 NA NA
#> 2 3  3 NA
complete_cases(x, "a")
#>   a  b  c
#> 1 1  1  1
#> 2 2 NA NA
#> 3 3  3 NA
#> 4 4  4  4
#> 5 5  5  5
complete_cases(x, "b")
#>   a b  c
#> 1 1 1  1
#> 2 3 3 NA
#> 3 4 4  4
#> 4 5 5  5
complete_cases(x, "c")
#>   a b c
#> 1 1 1 1
#> 2 4 4 4
#> 3 5 5 5
```
