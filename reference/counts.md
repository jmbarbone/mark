# Count observations by unique values

Variables will be return by the order in which they appear. Even factors
are shown by their order of appearance in the vector.

There are 2 methods for counting vectors. The `default` method uses
[`base::tabulate()`](https://rdrr.io/r/base/tabulate.html) (the
workhorse for [`base::table()`](https://rdrr.io/r/base/table.html) with
a call to
[`pseudo_id()`](https://jmbarbone.github.io/mark/reference/pseudo_id.md)
to transform all inputs into integers. The `logical` method counts
`TRUE`, `FALSE` and `NA` values, which is much quicker.

## Usage

``` r
counts(x, ...)

# S3 method for class 'data.frame'
counts(x, cols, sort = FALSE, ..., .name = "freq")

props(x, ...)

# Default S3 method
props(x, sort = FALSE, na.rm = FALSE, ...)

# S3 method for class 'data.frame'
props(x, cols, sort = FALSE, na.rm = FALSE, ..., .name = "prop")
```

## Arguments

- x:

  A vector or `data.frame`

- ...:

  Arguments passed to other methods

- cols:

  A vector of column names or indexes

- sort:

  Logical, if `TRUE` will sort values (not counts) before returning. For
  factors this will sort by factor levels. This has no effect for
  logical vectors, which already return in the order of `FALSE`, `TRUE`,
  `NA`.

- .name:

  The name of the new column

- na.rm:

  If `TRUE` will remove NA values from proportions

## Value

A named vector of `integer`s or `double`s (for `counts`, and `props`,
respectively) or `data.frame` with columns for each column chosen and
the `.name` chosen for the summary

## Details

Get counts or proportions of unique observations in a vector or columns
in a `data.frame`

## Examples

``` r
x <- sample(1:5, 10, TRUE)
counts(x)
#> 3 5 4 2 1 
#> 2 3 2 2 1 
props(x)
#>   3   5   4   2   1 
#> 0.2 0.3 0.2 0.2 0.1 

x <- quick_df(list(
  a = c("a", "c", "a", "c", "d", "b"),
  b = c("a", "a", "a", "c", "c", "b"),
  c = c("a", "a", "a", "c", "b", "b")
))

counts(x, "a")
#>   a freq
#> 1 a    2
#> 2 c    2
#> 3 d    1
#> 4 b    1
counts(x, c("a", "b", "c"))
#>   a b c freq
#> 1 a a a    2
#> 2 c a a    1
#> 3 c c c    1
#> 4 d c b    1
#> 5 b b b    1
props(x, 2)
#>   b      prop
#> 1 a 0.5000000
#> 2 c 0.3333333
#> 3 b 0.1666667
props(x, 1:3)
#>   a b c prop
#> 1 a a a  0.4
#> 2 c a a  0.2
#> 3 c c c  0.2
#> 4 d c b  0.2
#> 5 b b b  0.2

props(c(1, 1, 3, NA, 4))
#>    1    3    4 <NA> 
#>  0.4  0.2  0.2  0.2 
props(c(1, 1, 3, NA, 4), na.rm = TRUE)
#>    1    3    4 <NA> 
#> 0.50 0.25 0.25   NA 
```
