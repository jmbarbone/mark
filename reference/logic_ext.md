# Logic - Extensions

All functions take logical or logical-like (i.e., 1, 0, or NA as integer
or doubles) and return logical values.

Extensions to the base logical operations to account for `NA` values.

[`base::isTRUE()`](https://rdrr.io/r/base/Logic.html) and
[`base::isFALSE()`](https://rdrr.io/r/base/Logic.html) will only return
single length `TRUE` or `FALSE` as it checks for valid lengths in the
evaluation. When needing to check over a vector for the presence of
`TRUE` or `FALSE` and not being held back by `NA` values, `is_true` and
`is_false` will always provide a `TRUE` `FALSE` when the vector is
logical or return `NA` is the vector `x` is not logical.

`%xor%` is just a wrapper for
[`base::xor()`](https://rdrr.io/r/base/Logic.html)

## Usage

``` r
is_true(x)

# Default S3 method
is_true(x)

# S3 method for class 'logical'
is_true(x)

is_false(x)

# Default S3 method
is_false(x)

# S3 method for class 'logical'
is_false(x)

x %xor% y

nor(x, y)

nand(x, y)

xnandr(x, y)

OR(..., na.rm = FALSE)

AND(..., na.rm = FALSE)

either(x, y)

is_boolean(x)

none(..., na.rm = FALSE)

isNA(x)
```

## Arguments

- x, y:

  A vector of values. `%xor`, `nor`, `nand`, and `xnandr`

- ...:

  Vectors or a list of logical values

- na.rm:

  Logical, if `TRUE` will ignore `NA`

## Value

- `is_true()`, `is_false()`, `either()`, `%or%`, `nor`, `nand`, `xandr`,
  `AND()`, `OR()`: A `logical` vector, equal length of `x` (or `y` or of
  all `...` lengths)

- `is_boolean()`, `isNA()`, `none()`: A single `logical` value

## Details

Logical operations, extended

## Examples

``` r
x <- c(TRUE, FALSE, NA)
y <- c(FALSE, FALSE, TRUE)
z <- c(TRUE, NA, TRUE)
isTRUE(x)
#> [1] FALSE
is_true(x)
#> [1]  TRUE FALSE FALSE
isFALSE(x)
#> [1] FALSE
is_false(x)
#> [1] FALSE  TRUE FALSE
x %xor% TRUE
#> Warning: `%xor%` is deprecated. Please use `xor()` instead.
#> [1] FALSE  TRUE    NA
TRUE %xor% TRUE
#> Warning: `%xor%` is deprecated. Please use `xor()` instead.
#> [1] FALSE
TRUE %xor% FALSE
#> Warning: `%xor%` is deprecated. Please use `xor()` instead.
#> [1] TRUE
NA %xor% FALSE
#> Warning: `%xor%` is deprecated. Please use `xor()` instead.
#> [1] NA
OR(x, y, z)
#> [1] TRUE   NA TRUE
OR(x, y, z, na.rm = TRUE)
#> [1]  TRUE FALSE  TRUE
AND(x, y, z)
#> [1] FALSE FALSE    NA
AND(x, y, z, na.rm = TRUE)
#> [1] FALSE FALSE  TRUE
either(x, FALSE)
#> [1]  TRUE FALSE FALSE
either(TRUE, FALSE)
#> [1] TRUE
either(FALSE, NA)
#> [1] FALSE
either(TRUE, NA)
#> [1] TRUE
none(x)
#> [1] FALSE
none(x & y, na.rm = TRUE)
#> [1] TRUE
is_boolean(x)
#> [1] TRUE
is_boolean(c(1L, NA_integer_, 0L))
#> [1] TRUE
is_boolean(c(1.01, 0, -1))
#> [1] FALSE
```
