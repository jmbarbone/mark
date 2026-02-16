# That

Grammatical correctness

## Usage

``` r
that(x, arr.ind = FALSE, useNames = TRUE)
```

## Arguments

- x:

  a [`logical`](https://rdrr.io/r/base/logical.html) vector or array.
  [`NA`](https://rdrr.io/r/base/NA.html)s are allowed and omitted
  (treated as if `FALSE`).

- arr.ind:

  logical; should **arr**ay **ind**ices be returned when `x` is an
  array? Anything other than a single true value is treated as false.

- useNames:

  logical indicating if the value of
  [`arrayInd()`](https://rdrr.io/r/base/which.html) should have
  (non-null) dimnames at all.

## Value

see [`base::which()`](https://rdrr.io/r/base/which.html)

## Details

See `fortunes::fortune(175)`.

## See also

[`base::which()`](https://rdrr.io/r/base/which.html)
