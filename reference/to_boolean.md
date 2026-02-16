# To Boolean

Convert a vector to boolean/logical

## Usage

``` r
to_boolean(x, ...)

# S3 method for class 'logical'
to_boolean(x, ...)

# S3 method for class 'numeric'
to_boolean(x, true = 1, false = 0, na = NULL, ...)

# S3 method for class 'integer'
to_boolean(x, true = 1L, false = 0L, ...)

# S3 method for class 'character'
to_boolean(
  x,
  true = c("TRUE", "true", "T", "t", "YES", "yes", "Y", "y"),
  false = c("FALSE", "false", "F", "f", "NO", "no", "N", "n"),
  na = NULL,
  ...
)

# S3 method for class 'factor'
to_boolean(x, ...)
```

## Arguments

- x:

  A vector of values

- ...:

  Additional arguments passed to methods

- true:

  A vector of values to convert to `TRUE`

- false:

  A vector of values to convert to `FALSE`

- na:

  An optional vector of values to convert to `NA`; if set, an error will
  be thrown when any values in `x` are not matched to `true`, `false`,
  or `na`.

## Value

A `logical` vector of equal length as `x`.

## Details

`to_boolean.integer()` is passed to `to_boolean.numeric()` with
integer-specific `true` and `false` defaults. `to_boolean.factor()`
converts the factor levels via `to_boolean.character()`.
