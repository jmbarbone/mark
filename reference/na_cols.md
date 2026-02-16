# Selecting NA columns

Select or remove columns that are entirely NA

## Usage

``` r
select_na_cols(x)

remove_na_cols(x)

is_na_cols(x, names = TRUE)
```

## Arguments

- x:

  A `data.frame`

- names:

  Logical, if `TRUE` (default) will return column names as names of
  vector

## Value

- `select_na_cols()` `x` with only columns that are all `NA`

- `remove_na_cols()` `x` without columns of only `NA`

- `is_na_cols()` a logical vector: `TRUE` all rows of column are `NA`,
  otherwise `FALSE`
