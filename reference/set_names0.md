# Set names

Sets or removes names

## Usage

``` r
set_names0(x, nm = x)

names_switch(x)
```

## Arguments

- x:

  A vector of values

- nm:

  A vector of names

## Value

- `set_names0()`: `x` with `nm` values assigned to names (if `x` is
  `NULL`, `NULL` is returned)

- [`remove_names()`](https://jmbarbone.github.io/fuj/reference/names.html):
  `x` without names

- `names_switch()`: `character` vector of equal length `x` where names
  and values are switched
