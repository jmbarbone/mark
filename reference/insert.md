# Insert

Insert values at a position

## Usage

``` r
insert(x, positions, values)
```

## Arguments

- x:

  A vector of values

- positions:

  Integer of positions of `x` to insert `values`

- values:

  A vector of values to insert into `x`

## Value

A vector with the intended values inserted

## Examples

``` r
insert(letters[1:5], c(2, 4), c("X", "Y"))
#> [1] "a" "X" "b" "c" "Y" "d" "e"
```
