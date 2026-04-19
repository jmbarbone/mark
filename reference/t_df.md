# Data frame transpose

This transposes a data.frame with
[`base::t()`](https://rdrr.io/r/base/t.html) but transforms back into a
data.frame with column and row names cleaned up. Because the data types
may be mixed and reduced to characters, this may only be useful for a
visual viewing of the data.frame.

## Usage

``` r
t_df(x)
```

## Arguments

- x:

  A data.frame

## Value

A transposed `data.frame` with columns (`"colname"`, `"row_1"`, `...`,
for each row in `x`.

## Details

Transposes a `data.frame` as a `data.frame`

## Examples

``` r
x <- data.frame(col_a = Sys.Date() + 1:5, col_b = letters[1:5], col_c = 1:5)
t_df(x)
#>   colname      row_1      row_2      row_3      row_4      row_5
#> 1   col_a 2026-04-20 2026-04-21 2026-04-22 2026-04-23 2026-04-24
#> 2   col_b          a          b          c          d          e
#> 3   col_c          1          2          3          4          5
```
