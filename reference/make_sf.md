# Make system file function

Simple wrapper for package specific function for internal packages.

## Usage

``` r
make_sf(package)
```

## Arguments

- package:

  The name of the package

## Value

A `function` wrapping
[`system.file()`](https://rdrr.io/r/base/system.file.html) which will
always use the package name provided in `package`

## Examples

``` r
make_sf("mark")()
#> [1] "/home/runner/work/_temp/Library/mark"
```
