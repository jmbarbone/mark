# Create an ID for a vector

Transforms a vector into an integer of IDs.

## Usage

``` r
pseudo_id(x, ...)

# S3 method for class 'pseudo_id'
pseudo_id(x, ...)

# Default S3 method
pseudo_id(x, na_last = TRUE, ...)

# S3 method for class 'factor'
pseudo_id(x, ...)
```

## Arguments

- x:

  A vector of values

- ...:

  Additional arguments passed to methods

- na_last:

  `Logical` if `FALSE` will not place `NA` at the end

## Value

A `pseudo_id` object where the `integer` value of the vector correspond
to the position of the unique values in the attribute `"uniques"`.

## Examples

``` r
set.seed(42)
(x <- sample(letters, 10, TRUE))
#>  [1] "q" "e" "a" "y" "j" "d" "r" "z" "q" "o"
(pid <- pseudo_id(x))
#>  [1] 1 2 3 4 5 6 7 8 1 9
#> Uniques: q e a y j d r z o 
attr(pid, "uniques")[pid]
#>  [1] "q" "e" "a" "y" "j" "d" "r" "z" "q" "o"
```
