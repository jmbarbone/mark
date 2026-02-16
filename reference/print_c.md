# Print as c

Prints a vector to paste into an R script

## Usage

``` r
print_c(x = read_clipboard(), sorted = TRUE, null = TRUE)
```

## Arguments

- x:

  A vector (defaults to reading the clipboard)

- sorted:

  If `TRUE` (default) applies
  [`sort()`](https://rdrr.io/r/base/sort.html) to `x`

- null:

  If `TRUE` (default) adds `NULL` at the end of the
  [`c()`](https://rdrr.io/r/base/c.html) print

## Value

Invisibly, as a `character` vector, the object printed to the console

## Details

This sorts (if set) and provides unique values for each element in `x`
and prints then as a call to `c`. This can be useful for copying data
that you want to save as a vector in an R script. The result is both
called in [`cat()`](https://rdrr.io/r/base/cat.html) as well as copied
to the clipboard.

## Examples

``` r
print_c(1:10)
#> c(
#> 1,
#> 2,
#> 3,
#> 4,
#> 5,
#> 6,
#> 7,
#> 8,
#> 9,
#> 10,
#> NULL
#> )
print_c(letters[1:3])
#> c(
#> "a",
#> "b",
#> "c",
#> NULL
#> )
print_c(month.abb)
#> c(
#> "Apr",
#> "Aug",
#> "Dec",
#> "Feb",
#> "Jan",
#> "Jul",
#> "Jun",
#> "Mar",
#> "May",
#> "Nov",
#> "Oct",
#> "Sep",
#> NULL
#> )
```
