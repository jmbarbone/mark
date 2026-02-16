# Match params

Much like [`base::match.arg()`](https://rdrr.io/r/base/match.arg.html)
with a few key differences:

- Will not perform partial matching

- Will not return error messages with ugly quotation marks

## Usage

``` r
match_param(
  param,
  choices,
  null = TRUE,
  partial = getOption("mark.match_param.partial", FALSE),
  multiple = FALSE,
  simplify = TRUE
)
```

## Arguments

- param:

  The parameter

- choices:

  The available choices; named lists will return the name (a character)
  for when matched to the value within the list element. A list of
  formula objects (preferred) retains the LHS of the formula as the
  return value when matched to the RHS of the formula.

- null:

  If `TRUE` allows `NULL` to be passed a `param`

- partial:

  If `TRUE` allows partial matching via
  [`pmatch()`](https://rdrr.io/r/base/pmatch.html)

- multiple:

  If `TRUE` allows multiple values to be returned

- simplify:

  If `TRUE` will simplify the output to a single value

## Value

A single value from `param` matched on `choices`

## Details

Param matching for an argument

## See also

[`match_arg()`](https://jmbarbone.github.io/mark/reference/match_arg.md)

## Examples

``` r
fruits <- function(x = c("apple", "banana", "orange")) {
  match_param(x)
}

fruits()         # apple
#> [1] "apple"
try(fruits("b")) # must be exact fruits("banana")
#> Error : <matchParamMatchError> `match_param(x)` failed in `fruits("b")`:
#>   param    b
#>   choices  apple, banana, orange
#> package:mark

pfruits <- function(x = c("apple", "apricot", "banana")) {
  match_param(x, partial = TRUE)
}
pfruits()          # apple
#> [1] "apple"
try(pfruits("ap")) # matchParamMatchError
#> Error : <matchParamMatchError> `match_param(x)` failed in `pfruits("ap")`:
#>   param    ap
#>   choices  apple, apricot, banana
#> package:mark
pfruits("app")     # apple
#> [1] "apple"

afruits <- function(x = c("apple", "banana", "orange")) {
  match_param(x, multiple = TRUE)
}

afruits() # apple, banana, orange
#> [1] "apple"  "banana" "orange"

# can have multiple responses
how_much <- function(x = list(too_few = 0:2, ok = 3:5, too_many = 6:10)) {
  match_param(x)
}

how_much(1)
#> [1] "too_few"
how_much(3)
#> [1] "ok"
how_much(9)
#> [1] "too_many"

# use a list of formulas instead
ls <- list(1L ~ 0:1, 2L, 3L ~ 3:5)
sapply(0:5, match_param, choices = ls)
#> [1] 1 1 2 3 3 3
```
