# To row names

Converts a column to row names

## Usage

``` r
to_row_names(data, row_names = 1L)
```

## Arguments

- data:

  A data.frame

- row_names:

  The numeric position of the column.

## Value

A `data.frame`

## Examples

``` r
x <- data.frame(
  a = 1:4,
  b = letters[1:4]
)

to_row_names(x)
#>   b
#> 1 a
#> 2 b
#> 3 c
#> 4 d
to_row_names(x, "b")
#>   a
#> a 1
#> b 2
#> c 3
#> d 4
```
