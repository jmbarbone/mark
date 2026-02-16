# `fact` with `NA`

Included `NA` values into
[`fact()`](https://jmbarbone.github.io/mark/reference/fact.md)

## Usage

``` r
fact_na(x, remove = FALSE)
```

## Arguments

- x:

  A `fact` or object cohered to `fact`

- remove:

  If `TRUE` removes `NA` value from the `fact` `levels` and `uniques`
  attributes

## Value

A `fact` vector

## Details

This re-formats the `x` value so that `NA`s are found immediately within
the object rather than accessed through its attributes.

## See also

Other factors:
[`as_ordered()`](https://jmbarbone.github.io/mark/reference/as_ordered.md),
[`char2fact()`](https://jmbarbone.github.io/mark/reference/char2fact.md),
[`drop_levels()`](https://jmbarbone.github.io/mark/reference/drop_levels.md),
[`fact()`](https://jmbarbone.github.io/mark/reference/fact.md),
[`fact2char()`](https://jmbarbone.github.io/mark/reference/fact2char.md)
