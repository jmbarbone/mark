# Blank values

Detect *blank* values; select, remove columns that are entirely *blank*

## Usage

``` r
is_blank(x, na_blank = FALSE, ws = TRUE)

select_blank_cols(x, na_blank = FALSE, ws = TRUE)

remove_blank_cols(x, na_blank = FALSE, ws = TRUE)

is_blank_cols(x, names = TRUE, na_blank = FALSE, ws = TRUE)
```

## Arguments

- x:

  An object, or `data.frame` for `*_cols()` functions

- na_blank:

  Logical, if `TRUE` treats `NA` values as *blank*

- ws:

  Logical, when `TRUE` treats elements that are entirely *whitespace* as
  blanks

- names:

  Logical, if `TRUE` (default) will return column names as names of
  vector

## Value

- `is_blank()` a `logical` vector indicating *blank* elements in `x`

- `select_blank_cols()` `x` with only columns that are all *blank*

- `remove_blank_cols()` `x` without columns of only *blank*

- `is_blank_cols()` a logical vector: `TRUE` all rows of column are
  *blank*, otherwise `FALSE`

## Details

*Blank* values are values that do not contain any text
