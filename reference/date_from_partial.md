# Partial dates

Derive a date vector from a partial date string

## Usage

``` r
date_from_partial(
  x,
  format = "ymd",
  method = c("min", "max"),
  year_replacement = NA_integer_
)
```

## Arguments

- x:

  A vector of dates written as characters

- format:

  Format order of the date (accepts only combinations of `'y'`, `'m'`,
  and `'d'`)

- method:

  Method for reporting partial dates as either the earliest possible
  date (`"min"`) or the latest possible date (`"max"`); dates with
  missing days will be adjusted accordingly to the month and, if needed,
  the leap year

- year_replacement:

  (Default: `NA_integer_`) If set, will use this as a replacement for
  dates that contain missing years

## Value

A vector of `Dates`

## Details

Takes a character as an argument and attempts to create a date object
when part of the date string is missing.

## Examples

``` r
x <- c("2020-12-17", NA_character_, "", "2020-12-UN", "2020-12-UN",
       "2019-Unknown-00", "UNK-UNK-UNK", "1991-02-UN", "    ",
       "2020January20")
data.frame(
  x = x,
  min = date_from_partial(x),
  max = date_from_partial(x, method = "max"),
  year = date_from_partial(x, year_replacement = 1900)
)
#>                  x        min        max       year
#> 1       2020-12-17 2020-12-17 2020-12-17 2020-12-17
#> 2             <NA>       <NA>       <NA>       <NA>
#> 3                        <NA>       <NA>       <NA>
#> 4       2020-12-UN 2020-12-01 2020-12-31 2020-12-01
#> 5       2020-12-UN 2020-12-01 2020-12-31 2020-12-01
#> 6  2019-Unknown-00 2019-01-01 2019-12-31 2019-01-01
#> 7      UNK-UNK-UNK       <NA>       <NA> 1900-01-01
#> 8       1991-02-UN 1991-02-01 1991-02-28 1991-02-01
#> 9                        <NA>       <NA>       <NA>
#> 10   2020January20 2020-01-20 2020-01-20 2020-01-20
```
