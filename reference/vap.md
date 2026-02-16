# Vaps!

Wrappers for vapply

## Usage

``` r
vap_int(.x, .f, ..., .nm = FALSE)

vap_dbl(.x, .f, ..., .nm = FALSE)

vap_chr(.x, .f, ..., .nm = FALSE)

vap_lgl(.x, .f, ..., .nm = FALSE)

vap_cplx(.x, .f, ..., .nm = FALSE)

vap_date(.x, .f, ..., .nm = FALSE)
```

## Arguments

- .x:

  A vector of values

- .f:

  A function to apply to each element in vector `.x`

- ...:

  Additional arguments passed to `.f`

- .nm:

  Logical, if `TRUE` returns names of `.x` (Note: If `.x` does not have
  any names, they will be set to the values)

## Value

A vector of type matching the intended value in the function name.

## Details

These are simply wrappers for
[`base::vapply()`](https://rdrr.io/r/base/lapply.html) to shorten lines.

Each function is designed to use specific vector types:

- vap_int:

  integer

- vap_dbl:

  double

- vap_chr:

  character

- vap_lgl:

  logical

- vap_cplx:

  complex

- vap_date:

  Date

## See also

[`base::vapply()`](https://rdrr.io/r/base/lapply.html)
