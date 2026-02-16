# Array extract

Extract dimensions from an array

## Usage

``` r
array_extract(.arr, ..., default = "1")
```

## Arguments

- .arr:

  An array

- ...:

  A named list by array dimension number and the value

- default:

  The default dimension index

## Value

A value from the array `arr`

## Examples

``` r
x <- array(rep(NA, 27), dim = c(3, 3, 3))
x[1, 2, 3] <- TRUE
x[1, 2, 3]
#> [1] TRUE
x
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]   NA   NA   NA
#> [2,]   NA   NA   NA
#> [3,]   NA   NA   NA
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]   NA   NA   NA
#> [2,]   NA   NA   NA
#> [3,]   NA   NA   NA
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3]
#> [1,]   NA TRUE   NA
#> [2,]   NA   NA   NA
#> [3,]   NA   NA   NA
#> 
array_extract(x, `2` = 2, `3` = 3)
#> [1] TRUE
```
