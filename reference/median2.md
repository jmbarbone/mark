# Median (Q 50)

Median as the 50th quantile with an option to select quantile algorithm

## Usage

``` r
median2(x, type = 7, na.rm = FALSE)

q50(x, type = 7, na.rm = FALSE)
```

## Arguments

- x:

  numeric vector whose sample quantiles are wanted, or an object of a
  class for which a method has been defined (see also ‘details’).
  [`NA`](https://rdrr.io/r/base/NA.html) and `NaN` values are not
  allowed in numeric vectors unless `na.rm` is `TRUE`.

- type:

  an integer between 1 and 9 selecting one of the nine quantile
  algorithms detailed below to be used.

- na.rm:

  logical; if true, any [`NA`](https://rdrr.io/r/base/NA.html) and
  `NaN`'s are removed from `x` before the quantiles are computed.

## Value

See [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html)

## Details

`q50` is an alias for `median2`

## See also

[`stats::quantile()`](https://rdrr.io/r/stats/quantile.html)

## Examples

``` r
set.seed(42)
x <- rnorm(100)
median(x)            # 0.08979677
#> [1] 0.08979677
median2(x, type = 7) # 0.08979677 - default type is 7
#> [1] 0.08979677
median2(x, type = 3) # 0.08976065
#> [1] 0.08976065
```
