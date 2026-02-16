# Reindex a data.frame

Reindexes a data.frame with a reference

## Usage

``` r
reindex(
  x,
  index = NULL,
  new_index,
  expand = c("intersect", "both"),
  sort = FALSE
)
```

## Arguments

- x:

  A data.frame

- index:

  The column name or number of an index to use; if `NULL` will assume
  the first column; a value of `row.names` will use `row.names(x)`

- new_index:

  A column vector of the new index value

- expand:

  Character switch to expand or keep only the values that intersect
  (none), all values in x or index, or retain all values found.

- sort:

  Logical, if `TRUE` will sort the rows in output

## Value

A `data.frame` with rows of `index`

## Examples

``` r
iris1 <- head(iris, 5)
iris1$index <- 1:5
reindex(iris1, "index", seq(2, 8, 2))
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species index
#> 2          4.9         3.0          1.4         0.2  setosa     2
#> 4          4.6         3.1          1.5         0.2  setosa     4
reindex(iris1, "index", seq(2, 8, 2), expand = "both")
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species index
#> 1          5.1         3.5          1.4         0.2  setosa     1
#> 2          4.9         3.0          1.4         0.2  setosa     2
#> 3          4.7         3.2          1.3         0.2  setosa     3
#> 4          4.6         3.1          1.5         0.2  setosa     4
#> 5          5.0         3.6          1.4         0.2  setosa     5
#> 6           NA          NA           NA          NA    <NA>    NA
#> 8           NA          NA           NA          NA    <NA>    NA

# Using letters will show changes in rownames
iris1$index <- letters[1:5]
reindex(iris1, "index", letters[seq(2, 8, 2)])
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species index
#> b           NA          NA           NA          NA    <NA>  <NA>
#> d           NA          NA           NA          NA    <NA>  <NA>
reindex(iris1, "index", seq(2, 8, 2))
#> [1] Sepal.Length Sepal.Width  Petal.Length Petal.Width  Species     
#> [6] index       
#> <0 rows> (or 0-length row.names)
reindex(iris1, "index", seq(2, 8, 2), expand = "both")
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species index
#> a           NA          NA           NA          NA    <NA>  <NA>
#> b           NA          NA           NA          NA    <NA>  <NA>
#> c           NA          NA           NA          NA    <NA>  <NA>
#> d           NA          NA           NA          NA    <NA>  <NA>
#> e           NA          NA           NA          NA    <NA>  <NA>
#> 2          4.9         3.0          1.4         0.2  setosa     b
#> 4          4.6         3.1          1.5         0.2  setosa     d
#> 6           NA          NA           NA          NA    <NA>  <NA>
#> 8           NA          NA           NA          NA    <NA>  <NA>
```
