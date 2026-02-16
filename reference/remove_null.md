# Remove NULL

Remove NULL results from a list

## Usage

``` r
remove_null(x)
```

## Arguments

- x:

  A list

## Value

The list `x` without `NULL`

## Examples

``` r
x <- list(a = letters[1:5], b = NULL, c = complex(3))
x
#> $a
#> [1] "a" "b" "c" "d" "e"
#> 
#> $b
#> NULL
#> 
#> $c
#> [1] 0+0i 0+0i 0+0i
#> 
remove_null(x)
#> $a
#> [1] "a" "b" "c" "d" "e"
#> 
#> $c
#> [1] 0+0i 0+0i 0+0i
#> 
```
