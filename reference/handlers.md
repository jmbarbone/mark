# Handlers

Catch and report handlers

## Usage

``` r
has_warning(x, FUN, ...)

has_error(x, FUN, ...)

has_message(x, FUN, ...)

get_warning(x, FUN, ..., .null = TRUE)

get_message(x, FUN, ..., .null = TRUE)

get_error(x, FUN, ..., .null = TRUE)
```

## Arguments

- x:

  A vector

- FUN:

  A function

- ...:

  Additional params passed to `FUN`

- .null:

  Logical, if `FALSE` will drop `NULL` results (for `get_*()`)

## Value

The `has_*()` functions will return `TRUE`/`FALSE` for if the handler is
found in the execution of the code. The `get_*()` functions provide the
text of the message

## Details

These functions can be used to catch whether an evaluation will return
an error or warning without raising.

## References

Function for *catching* has been adapted from
https://stackoverflow.com/a/4952908/12126576

## Examples

``` r
has_warning(c(1, "no"), as.integer)
#>     1    no 
#> FALSE  TRUE 
#     1    no
# FALSE  TRUE

get_warning(c(1, "no"), as.integer)
#> $`1`
#> NULL
#> 
#> $no
#> [1] "NAs introduced by coercion"
#> 

# drop NULLs
get_warning(c(1, "no"), as.integer, .null = FALSE)
#> $no
#> [1] "NAs introduced by coercion"
#> 

foo <- function(x) {
  stopifnot(x > 0)
  x
}

has_error(c(1, 0, 2), foo)
#>     1     0     2 
#> FALSE  TRUE FALSE 
#     1     0     2
# FALSE  TRUE FALSE

get_error(c(1, 0, 2), foo)
#> $`1`
#> NULL
#> 
#> $`0`
#> [1] "x > 0 is not TRUE"
#> 
#> $`2`
#> NULL
#> 

# drop NULLs
get_error(c(1, 0, 2), foo, .null = FALSE)
#> $`0`
#> [1] "x > 0 is not TRUE"
#> 
```
