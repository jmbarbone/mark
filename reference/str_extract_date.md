# Extract date from string

Extract date from string

## Usage

``` r
str_extract_date(x, format = "%Y-%m-%d")

str_extract_datetime(x, format = "%Y-%m-%d %H%M%S")
```

## Arguments

- x:

  A character vector

- format:

  A date format to find

## Value

A `Date` (if found) or `NA`

## Examples

``` r
str_extract_date("This is a file name 2020-02-21.csv")
#> [1] "2020-02-21"
str_extract_date(c("This is a file name 2020-02-21.csv",
                   "Date of 2012-06-15 here"))
#> [1] "2020-02-21" "2012-06-15"
str_extract_date(c("This is a file name 2020-02-21.csv", "No date"))
#> [1] "2020-02-21" NA          
str_extract_date("Last saved 17 December 2019", format = "%d %B %Y")
#> [1] "2019-12-17"

str_extract_datetime(c("2020-02-21 235033", "2012-12-12 121212"))
#> [1] "2020-02-21 23:50:33 UTC" "2012-12-12 12:12:12 UTC"
str_extract_datetime("This is a file name 2020-02-21 235033.csv")
#> [1] "2020-02-21 23:50:33 UTC"
```
