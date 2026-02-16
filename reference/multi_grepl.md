# Multiple searching

Multiple search pattern searches

## Usage

``` r
multi_grepl(x, patterns, ..., simplify = TRUE)

multi_grep(x, patterns, ..., simplify = TRUE)
```

## Arguments

- x:

  Passed to [`base::grepl()`](https://rdrr.io/r/base/grep.html)

- patterns:

  A list or vector of patterns to search across `x`; if named value
  returned will be the name of the pattern – otherwise the position.
  Pattern match reported will be the first in the list that is found

- ...:

  Additional arguments passed to
  [`base::grepl()`](https://rdrr.io/r/base/grep.html)

- simplify:

  if `FALSE` will return a list of all matches, otherwise the first
  match found

## Value

The name or position of the pattern that is matched

## Examples

``` r
x <- c("apple", "banana", "lemon")
multi_grepl(x, c("a" = "^[ab]", "b" = "lem"))
#> [1] "a" "a" "b"
multi_grepl(x, c("a" = "^[ab]", "b" = "q"))                   # lemon not matches on either
#> [1] "a" "a" NA 
multi_grepl(x, c("a" = "^[ab]", "b" = "e"))                   # apple matches "a" before "b"
#> [1] "a" "a" "b"
multi_grepl(x, c("a" = "^[ab]", "b" = "e"), simplify = FALSE) # shows all matches
#> [[1]]
#> [1] "a" "b"
#> 
#> [[2]]
#> [1] "a"
#> 
#> [[3]]
#> [1] "b"
#> 
multi_grepl(x, c("^[ab]", "e"))                               # returned as positions
#> [1] 1 1 2
multi_grepl(x, c("^[ab]", "e"), simplify = FALSE)
#> [[1]]
#> [1] 1 2
#> 
#> [[2]]
#> [1] 1
#> 
#> [[3]]
#> [1] 2
#> 
```
