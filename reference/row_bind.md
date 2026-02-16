# Row bind

Bind a list of `data.frames`

## Usage

``` r
row_bind(...)
```

## Arguments

- ...:

  A list of `data.frames` to be attached to each other by row

## Value

A `data.frame` combining all the rows from `data.frame`s in `...` and
all the columns, as they appear. An empty `data.frame` with `0` columns
and `0` rows is returned if `...` has no length

## See also

[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
[`base::rbind()`](https://rdrr.io/r/base/cbind.html)
