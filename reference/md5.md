# Compute the MD5 hash of an object

Wrapper for calling
[`tools::md5sum()`](https://rdrr.io/r/tools/md5sum.html) on objects
rather than files.

## Usage

``` r
md5(x, bytes = getOption("mark.md5.bytes"))
```

## Arguments

- x:

  An object

- bytes:

  If `TRUE` will use the `bytes` argument in
  [`tools::md5sum()`](https://rdrr.io/r/tools/md5sum.html), available in
  R \>= 4.5.0. If `NA` (the default) will use `TRUE` if available.

## Value

A `md5sum` object

## Details

All `x` objects are [serialized](https://rdrr.io/r/base/serialize.html)
to a temporary file before
[`tools::md5sum()`](https://rdrr.io/r/tools/md5sum.html) is called.

## Examples

``` r
md5("hello")
#> 6c0ab239b664493e860c1619ed379898
md5(1:10)
#> 80e76ade6af799c5b55e6447e7bf0d7b
md5(data.frame(a = 1:10, b = letters[1:10]))
#> 088023b9c6c6ae5f2e7997a91de9b7c0
```
