# Limit

Limit a numeric vector by lower and upper bounds

## Usage

``` r
limit(x, lower = min(x), upper = max(x))
```

## Arguments

- x:

  A numeric vector

- lower:

  A lower limit (as `x < lower`)

- upper:

  An upper limit (as `x > higher`)

## Value

The vector `x` with `lower` and `upper` as the minimum, maximum values
