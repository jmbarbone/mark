# List to data.frame

Converts a list object into a `data.frame`

## Usage

``` r
list2df(x, name = "name", value = "value", warn = TRUE)
```

## Arguments

- x:

  A (preferably) named `list` with any number of values

- name, value:

  Names of the new key and value columns, respectively

- warn:

  Logical; if `TRUE` will show a warning when

## Value

a `data.frame` object with columns `"name"` and `"value"` for the names
of the `list` and the values in each

## Details

Unlike [`base::list2DF()`](https://rdrr.io/r/base/list2DF.html),
`list2df()` tries to format the `data.frame` by using the names of the
list as values rather than variables. This creates a longer form list
that may be more tidy.

## Examples

``` r
x <- list(a = 1, b = 2:4, c = letters[10:20], "unnamed", "unnamed2")
list2df(x, "col1", "col2", warn = FALSE)
#>    col1     col2
#> 1     a        1
#> 2     b        2
#> 3     b        3
#> 4     b        4
#> 5     c        j
#> 6     c        k
#> 7     c        l
#> 8     c        m
#> 9     c        n
#> 10    c        o
#> 11    c        p
#> 12    c        q
#> 13    c        r
#> 14    c        s
#> 15    c        t
#> 16    4  unnamed
#> 17    5 unnamed2

# contrast with `base::list2DF()` and `base::as.data.frame()`
x <- list(a = 1:3, b = 2:4, c = letters[10:12])
list2df(x, warn = FALSE)
#>   name value
#> 1    a     1
#> 2    a     2
#> 3    a     3
#> 4    b     2
#> 5    b     3
#> 6    b     4
#> 7    c     j
#> 8    c     k
#> 9    c     l
list2DF(x)
#>   a b c
#> 1 1 2 j
#> 2 2 3 k
#> 3 3 4 l
as.data.frame(x)
#>   a b c
#> 1 1 2 j
#> 2 2 3 k
#> 3 3 4 l
```
