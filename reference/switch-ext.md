# Switch with a list of parameters

`switch_params()` is a vectorized version of `switch` `switch_case()`
uses a formula syntax to return the value to the right of the tilde
(`~`) when `x` is `TRUE` `switch_in_case()` is a special case of
`switch_case()` for [`match()`](https://rdrr.io/r/base/match.html)-ing
`x` in the values on the left to return the value on the right.

## Usage

``` r
switch_params(x, ...)

switch_in_case(x, ..., .default = NULL, .envir = parent.frame())

switch_case(..., .default = NULL, .envir = parent.frame())
```

## Arguments

- x:

  A vector of values

- ...:

  Case evaluations (named for `switch_params`)

- .default:

  The default value if no matches are found in `...` (default: `NULL`
  produces an `NA` value derived from `...`)

- .envir:

  The environment in which to evaluate the LHS of `...` (default:
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html))

## Value

A named vector of values of same length `x`; or for `switch_case`, an
unnamed vector of values matching the rhs of `...`

Inspired from:

- https://stackoverflow.com/a/32835930/12126576

- https://github.com/tidyverse/dplyr/issues/5811

## Details

Switch with a list of params

## Examples

``` r
# by single
switch_params(c("j", "m", "b"), j = 10, b = 2, m = 13)
#>  j  m  b 
#> 10 13  2 


# match with TRUE
switch_case(
  1:10 == 9      ~ NA_integer_,
  1:10 %% 3 == 0 ~ 1:10,
  1:10 %% 4 == 0 ~ 11:20,
  1:10 %% 5 == 0 ~ 21:30,
  1:10 %% 2 == 0 ~ 31:40,
  .default = -1L
)
#>  [1] -1 32  3 14 25  6 -1 18 NA 30

# match within a vector
switch_in_case(
  c(1, 2, 12, 4, 20, 21),
  1:10  ~ 1,
  11:20 ~ 2
)
#>  1  2 12  4 20 21 
#>  1  1  2  1  2 NA 

switch_in_case(
  c("a", "b", "d", "e", "g", "j"),
  letters[1:3] ~ "a",
  letters[5:6] ~ "e"
)
#>   a   b   d   e   g   j 
#> "a" "a"  NA "e"  NA  NA 

use_these <- c(1, 3, 2, 5)
switch_in_case(
  1:10,
  use_these ~ TRUE,
  .default = FALSE
)
#>     1     2     3     4     5     6     7     8     9    10 
#>  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE 

ne <- new.env()
ne$use_these2 <- use_these
# error
try(switch_in_case(
  1:10,
  use_these2 ~ TRUE
))
#> Error in eval(parse(text = i[1L]), envir = .envir) : 
#>   object 'use_these2' not found
switch_in_case(
  1:10,
  use_these2 ~ TRUE,
  .envir = ne
)
#>    1    2    3    4    5    6    7    8    9   10 
#> TRUE TRUE TRUE   NA TRUE   NA   NA   NA   NA   NA 

switch_in_case(
  seq.int(1, 60, 6),
  1:10          ~ "a",
  11:20         ~ "b",
  c(22, 24, 26) ~ "c",
  30:Inf        ~ "d"
)
#>   1   7  13  19  25  31  37  43  49  55 
#> "a" "a" "b" "b"  NA "d" "d" "d" "d" "d" 

# Use functions
switch_in_case(
  1:6,
  c(1, 3, 5) ~ exp,
  c(2, 4) ~ log
)
#>           1           2           3           4           5           6 
#>   2.7182818   0.6931472  20.0855369   1.3862944 148.4131591          NA 
```
