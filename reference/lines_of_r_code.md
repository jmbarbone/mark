# Lines of R code

Find the total number of lines of R code

## Usage

``` r
lines_of_r_code(x = ".", skip_empty = TRUE)
```

## Arguments

- x:

  Directory to search for files

- skip_empty:

  Logical, if TRUE will not count lines that are empty or only contain a
  bracket or quotation mark.

## Value

An `integer` for the number of lines in all applicable files

## Details

Tries to read each file in the directory that ends in .R or .r and sums
together. Files that fail to read are not counted.

## Examples

``` r
# \donttest{
lines_of_r_code(system.file())
#> [1] 292
lines_of_r_code(system.file(), skip_empty = FALSE)
#> [1] 376
# }
```
