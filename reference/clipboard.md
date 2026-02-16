# Write to and read from the clipboard

Wrappers for working with the clipboard

## Usage

``` r
write_clipboard(x, ...)

# Default S3 method
write_clipboard(x, ...)

# S3 method for class 'data.frame'
write_clipboard(x, sep = "\t", row.names = FALSE, ...)

# S3 method for class 'matrix'
write_clipboard(x, sep = "\t", ...)

# S3 method for class 'list'
write_clipboard(x, sep = "\t", ...)

read_clipboard(method = read_clipboard_methods(), ...)

read_clipboard_methods()
```

## Arguments

- x:

  An object

- ...:

  Additional arguments sent to methods or to
  [`utils::write.table()`](https://rdrr.io/r/utils/write.table.html)

- sep:

  the field separator string. Values within each row of `x` are
  separated by this string.

- row.names:

  either a logical value indicating whether the row names of `x` are to
  be written along with `x`, or a character vector of row names to be
  written.

- method:

  Method switch for loading the clipboard

## Value

`write_clipboard()` None, called for side effects `read_clipboard()`
Either a vector, `data.frame`, or `tibble` depending on the `method`
chosen. Unlike
[`utils::readClipboard()`](https://rdrr.io/r/utils/clipboard.html), an
empty clipboard value returns `NA` rather than `""`

## Details

As these functions rely on
[`clipr::read_clip()`](http://matthewlincoln.net/clipr/reference/read_clip.md)
and [`utils::writeClipboard()`](https://rdrr.io/r/utils/clipboard.html)
they are only available for Windows 10. For copying and pasting floats,
there may be some rounding that can occur.

## Examples

``` r
# Will only run on windows
if (Sys.info()[["sysname"]] == "Windows") {
  foo <- function(x) {
    write_clipboard(x)
    y <- read_clipboard()
    res <- all.equal(x, y)
    if (isTRUE(res)) return("All equal")
    print(x)
    print(y)
  }
  foo(1:4)
  foo(seq(-1, 1, .02))
  foo(Sys.Date() + 1:4)

  # May have some rounding issues
  x <- "0.316362437326461129"
  write_clipboard(x)
  res <- as.character(read_clipboard())
  all.equal(x, res)
  x; res
}
```
