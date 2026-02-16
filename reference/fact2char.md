# Factor to character

Convert factor columns to characters in a `data.frame`

## Usage

``` r
fact2char(data, threshold = 10)
```

## Arguments

- data:

  A `data.frame`

- threshold:

  A threshold for the number of levels to be met/exceeded for
  transforming into a character

## Value

The `data.frame` `data` with factors converted by the rule above

## See also

[`char2fact()`](https://jmbarbone.github.io/mark/reference/char2fact.md)

Other factors:
[`as_ordered()`](https://jmbarbone.github.io/mark/reference/as_ordered.md),
[`char2fact()`](https://jmbarbone.github.io/mark/reference/char2fact.md),
[`drop_levels()`](https://jmbarbone.github.io/mark/reference/drop_levels.md),
[`fact()`](https://jmbarbone.github.io/mark/reference/fact.md),
[`fact_na()`](https://jmbarbone.github.io/mark/reference/fact_na.md)
