# Vector to data.frame

Transforms a vector (named) to a data.frame

## Usage

``` r
vector2df(x, name = "name", value = "value", show_NA)
```

## Arguments

- x:

  A vector of values.

- name, value:

  Character strings for the name and value columns

- show_NA:

  Ignored; will trigger a warning if set

## Value

A `data.frame` with `name` (optional) and `value` columns
