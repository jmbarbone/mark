# Factor

Quickly create a factor

## Usage

``` r
fact(x)

# Default S3 method
fact(x)

# S3 method for class 'character'
fact(x)

# S3 method for class 'numeric'
fact(x)

# S3 method for class 'integer'
fact(x)

# S3 method for class 'Date'
fact(x)

# S3 method for class 'POSIXt'
fact(x)

# S3 method for class 'logical'
fact(x)

# S3 method for class 'factor'
fact(x)

# S3 method for class 'fact'
fact(x)

# S3 method for class 'pseudo_id'
fact(x)

# S3 method for class 'haven_labelled'
fact(x)
```

## Arguments

- x:

  A vector of values

## Value

A vector of equal length of `x` with class `fact` and `factor`. If `x`
was `ordered`, that class is added in between.

## Details

`fact()` can be about 5 times quicker than
[`factor()`](https://rdrr.io/r/base/factor.html) or
[`as.factor()`](https://rdrr.io/r/base/factor.html) as it doesn't bother
sorting the levels for non-numeric data or have other checks or
features. It simply converts a vector to a factor with all unique values
as levels with `NA`s included.

`fact.factor()` will perform several checks on a factor to include `NA`
levels and to check if the levels should be reordered to conform with
the other methods. The `fact.fact()` method simple returns `x`.

## level orders

The order of the levels may be adjusted to these rules depending on the
class of `x`:

- `character`:

  The order of appearance

- `numeric`/`integer`/`Date`/`POSIXt`:

  By the numeric order

- `logical`:

  As `TRUE`, `FALSE`, then `NA` if present

- `factor`:

  Numeric if levels can be safely converted, otherwise as they are

## See also

[`as_ordered()`](https://jmbarbone.github.io/mark/reference/as_ordered.md)

Other factors:
[`as_ordered()`](https://jmbarbone.github.io/mark/reference/as_ordered.md),
[`char2fact()`](https://jmbarbone.github.io/mark/reference/char2fact.md),
[`drop_levels()`](https://jmbarbone.github.io/mark/reference/drop_levels.md),
[`fact2char()`](https://jmbarbone.github.io/mark/reference/fact2char.md),
[`fact_na()`](https://jmbarbone.github.io/mark/reference/fact_na.md)
