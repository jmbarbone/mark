# Make not available

Create NA vectors

## Usage

``` r
not_available(type = "logical", length = 0L)

set_not_available(type, value)

NA_Date_

NA_POSIXct_

NA_POSIXlt_
```

## Format

An object of class `Date` of length 1.

An object of class `POSIXct` (inherits from `POSIXt`) of length 1.

An object of class `POSIXlt` (inherits from `POSIXt`) of length 1.

## Arguments

- type:

  Type of NA (see details)

- length:

  Length of the vector

- value:

  A value to return in `not_available()`

## Value

A vector of `NA` values

## Details

If length is a text it will search for an appropriate match.

## Examples

``` r
x <- not_available("Date", 3)
x
#> [1] NA NA NA
class(x)
#> [1] "Date"
```
