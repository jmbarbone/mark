# Table NA values

Tables out whether data are NAs are not

## Usage

``` r
tableNA(..., .list = FALSE)
```

## Arguments

- ...:

  one or more objects which can be interpreted as factors (including
  numbers or character strings), or a
  [`list`](https://rdrr.io/r/base/list.html) (such as a data frame)
  whose components can be so interpreted. (For `as.table`, arguments
  passed to specific methods; for `as.data.frame`, unused.)

- .list:

  Logical, if `TRUE` and `...` is a `list`, will c

## Value

[`table()`](https://rdrr.io/r/base/table.html) returns a *contingency
table*, an object of class `"table"`, an array of integer values. Note
that unlike S the result is always an
[`array`](https://rdrr.io/r/base/array.html), a 1D array if one factor
is given.

`as.table` and `is.table` coerce to and test for contingency table,
respectively.

The `as.data.frame` method for objects inheriting from class `"table"`
can be used to convert the array-based representation of a contingency
table to a data frame containing the classifying factors and the
corresponding entries (the latter as component named by `responseName`).
This is the inverse of [`xtabs`](https://rdrr.io/r/stats/xtabs.html).

## Details

All data are checked with [`is.na()`](https://rdrr.io/r/base/NA.html)
and the resulting `TRUE` or `FALSE` is are tabulated.

## References

Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) *The New S
Language*. Wadsworth & Brooks/Cole.

## See also

[`tabulate`](https://rdrr.io/r/base/tabulate.html) is the underlying
function and allows finer control.

Use [`ftable`](https://rdrr.io/r/stats/ftable.html) for printing (and
more) of multidimensional tables.
[`margin.table`](https://rdrr.io/r/base/marginSums.html),
[`prop.table`](https://rdrr.io/r/base/proportions.html),
[`addmargins`](https://rdrr.io/r/stats/addmargins.html).

[`addNA`](https://rdrr.io/r/base/factor.html) for constructing factors
with [`NA`](https://rdrr.io/r/base/NA.html) as a level.

[`xtabs`](https://rdrr.io/r/stats/xtabs.html) for cross tabulation of
data frames with a formula interface.

## Examples

``` r
x <- list(
  a = c(1, 2, NA, 3),
  b = c("A", NA, "B", "C"),
  c = as.Date(c("2020-01-02", NA, NA, "2020-03-02"))
)
tableNA(x) # entire list
#> x
#>  TRUE FALSE 
#>     0     3 
tableNA(x, .list = TRUE) # counts for each
#> , , c = TRUE
#> 
#>        b
#> a       TRUE FALSE
#>   TRUE     0     1
#>   FALSE    1     0
#> 
#> , , c = FALSE
#> 
#>        b
#> a       TRUE FALSE
#>   TRUE     0     0
#>   FALSE    0     2
#> 
tableNA(x[1], x[2])
#>        x[2]
#> x[1]    TRUE FALSE
#>   TRUE     0     0
#>   FALSE    0     1
tableNA(x[1], x[2], x[3]) # equivalent ot tableNA(x, .list = TRUE)
#> , , x[3] = TRUE
#> 
#>        x[2]
#> x[1]    TRUE FALSE
#>   TRUE     0     0
#>   FALSE    0     0
#> 
#> , , x[3] = FALSE
#> 
#>        x[2]
#> x[1]    TRUE FALSE
#>   TRUE     0     0
#>   FALSE    0     1
#> 
```
