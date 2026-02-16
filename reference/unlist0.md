# Unlist and squash

Unlist without unique names; combine names for unique values

## Usage

``` r
unlist0(x)

squash_vec(x, sep = ".")
```

## Arguments

- x:

  A vector of values

- sep:

  A separation for combining names

## Value

- `unlist0()`: a vector with the possibility of non-unique names

- `squash_vec()`: A vector of unique values and names

## Details

- `unlist0()` is much like
  [`unlist()`](https://rdrr.io/r/base/unlist.html) expect that name are
  not made to be unique.

- `squash_vec()` works differently

## Examples

``` r
x <- list(a = 1:3, b = 2, c = 2:4)
y <- c(a = 1, b = 1, c = 1, d = 2, e = 3, f = 3)

# unlist0() doesn't force unique names
unlist(x)   # names: a1 a2 a3  b c1 c2 c3
#> a1 a2 a3  b c1 c2 c3 
#>  1  2  3  2  2  3  4 
unlist0(x)  # names: a a a  b c c c
#> a a a b c c c 
#> 1 2 3 2 2 3 4 
unlist0(y)  # no change
#> a b c d e f 
#> 1 1 1 2 3 3 

# squash_vec() is like the inverse of unlist0() because it works on values
squash_vec(x)
#>     a a.b.c   a.c     c 
#>     1     2     3     4 
squash_vec(y)
#> a.b.c     d   e.f 
#>     1     2     3 
```
