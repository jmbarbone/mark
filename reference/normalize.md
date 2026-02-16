# Normalize values

Normalizes values based on possible range and new bounds

## Usage

``` r
normalize(x, ...)

# Default S3 method
normalize(x, range = base::range(x, na.rm = TRUE), bounds = 0:1, ...)

# S3 method for class 'data.frame'
normalize(x, ...)
```

## Arguments

- x:

  An object that is (coercible to) `double`; `data.frames` are
  transformed

- ...:

  Additional arguments passed to methods

- range:

  The range of possible values of `x`. See details for more info.
  Defaults to the range of non-`NA` values

- bounds:

  The new boundaries for the normalized values of `x`. Defaults to `0`
  and `1`.

## Value

`x` with transformed values where `range` values are transformed to
`bounds`.

## Details

Parameters `range` and `bounds` are modified with
[`base::range()`](https://rdrr.io/r/base/range.html). The largest and
smallest values are then used to determine the minimum/maximum values
and lower/upper bounds. This allows for a vector of more than two values
to be passed.

The current implementation of `normalize.data.frame()` allows for `list`
of parameters passed for each column. However, it is probably best
suited for default values.

## Examples

``` r
x <- c(0.23, 0.32, 0.12, 0.61, 0.26, 0.24, 0.23, 0.32, 0.29, 0.27)
data.frame(
  x  = normalize(x),
  v  = normalize(x, range = 0:2),
  b  = normalize(x, bounds = 0:10),
  vb = normalize(x, range = 0:2, bounds = 0:10)
)
#>            x     v         b   vb
#> 1  0.2244898 0.115  2.244898 1.15
#> 2  0.4081633 0.160  4.081633 1.60
#> 3  0.0000000 0.060  0.000000 0.60
#> 4  1.0000000 0.305 10.000000 3.05
#> 5  0.2857143 0.130  2.857143 1.30
#> 6  0.2448980 0.120  2.448980 1.20
#> 7  0.2244898 0.115  2.244898 1.15
#> 8  0.4081633 0.160  4.081633 1.60
#> 9  0.3469388 0.145  3.469388 1.45
#> 10 0.3061224 0.135  3.061224 1.35

# maintains matrix
mat <- structure(c(0.24, 0.92, 0.05, 0.37, 0.19, 0.69, 0.43, 0.22, 0.85,
0.73, 0.89, 0.68, 0.57, 0.89, 0.61, 0.98, 0.75, 0.37, 0.24, 0.24,
0.34, 0.8, 0.25, 0.46, 0.03, 0.71, 0.79, 0.56, 0.83, 0.97), dim = c(10L, 3L))

mat
#>       [,1] [,2] [,3]
#>  [1,] 0.24 0.89 0.34
#>  [2,] 0.92 0.68 0.80
#>  [3,] 0.05 0.57 0.25
#>  [4,] 0.37 0.89 0.46
#>  [5,] 0.19 0.61 0.03
#>  [6,] 0.69 0.98 0.71
#>  [7,] 0.43 0.75 0.79
#>  [8,] 0.22 0.37 0.56
#>  [9,] 0.85 0.24 0.83
#> [10,] 0.73 0.24 0.97
normalize(mat, bounds = -1:1)
#>             [,1]       [,2]        [,3]
#>  [1,] -0.5578947  0.8105263 -0.34736842
#>  [2,]  0.8736842  0.3684211  0.62105263
#>  [3,] -0.9578947  0.1368421 -0.53684211
#>  [4,] -0.2842105  0.8105263 -0.09473684
#>  [5,] -0.6631579  0.2210526 -1.00000000
#>  [6,]  0.3894737  1.0000000  0.43157895
#>  [7,] -0.1578947  0.5157895  0.60000000
#>  [8,] -0.6000000 -0.2842105  0.11578947
#>  [9,]  0.7263158 -0.5578947  0.68421053
#> [10,]  0.4736842 -0.5578947  0.97894737
normalize(as.data.frame(mat), bounds = -1:1)
#>            V1         V2          V3
#> 1  -0.5632184  0.7567568 -0.34042553
#> 2   1.0000000  0.1891892  0.63829787
#> 3  -1.0000000 -0.1081081 -0.53191489
#> 4  -0.2643678  0.7567568 -0.08510638
#> 5  -0.6781609  0.0000000 -1.00000000
#> 6   0.4712644  1.0000000  0.44680851
#> 7  -0.1264368  0.3783784  0.61702128
#> 8  -0.6091954 -0.6486486  0.12765957
#> 9   0.8390805 -1.0000000  0.70212766
#> 10  0.5632184 -1.0000000  1.00000000
```
