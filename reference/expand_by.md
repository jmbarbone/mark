# Expands a vector

Expands vector x by y

## Usage

``` r
expand_by(x, y, expand = c("x", "y", "intersect", "both"), sort = FALSE)
```

## Arguments

- x, y:

  Vectors

- expand:

  Character switch to expand or keep only the values that intersect, all
  values in x or y, or retain all values found.

- sort:

  Logical, if `TRUE` will sort by names in output

## Value

A vector with expanded

## Examples

``` r
x <- letters[c(3:2, 5, 9)]
y <- letters[c(1:4, 8)]
expand_by(x, y, "x")
#>   c   b   e   i 
#> "c" "b"  NA  NA 
expand_by(x, y, "y")
#>   a   b   c   d   h 
#>  NA "b" "c"  NA  NA 
expand_by(x, y, "intersect")
#>   b   c 
#> "b" "c" 
expand_by(x, y, "both")
#>   c   b   e   i   a   d   h 
#> "c" "b" "e" "i"  NA  NA  NA 
```
