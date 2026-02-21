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

  An object to write to the clipboard

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
Either a vector or `data.frame` (or `tibble`, if depending on the
`method` chosen. An empty clipboard value returns `NA` (rather than
`""`)

## Details

For copying and pasting floats, there may be some rounding that can
occur.

## Examples

``` r
# Will only run on windows
foo <- function(x) {
  write_clipboard(x)
  y <- read_clipboard()
  res <- all.equal(x, y)
  if (isTRUE(res)) return("All equal")
  print(x)
  print(y)
}

foo(1:4)
#> Error: Clipboard on X11 requires 'xclip' (recommended) or 'xsel'; Clipboard on Wayland requires 'wl-copy' and 'wl-paste'.
foo(seq(-1, 1, .02))
#> Error: Clipboard on X11 requires 'xclip' (recommended) or 'xsel'; Clipboard on Wayland requires 'wl-copy' and 'wl-paste'.
foo(Sys.Date() + 1:4)
#> Error: Clipboard on X11 requires 'xclip' (recommended) or 'xsel'; Clipboard on Wayland requires 'wl-copy' and 'wl-paste'.

# May have some rounding issues
x <- "0.316362437326461129"
write_clipboard(x)
#> Error: Clipboard on X11 requires 'xclip' (recommended) or 'xsel'; Clipboard on Wayland requires 'wl-copy' and 'wl-paste'.
res <- as.character(read_clipboard())
#> Error: Clipboard on X11 requires 'xclip' (recommended) or 'xsel'; Clipboard on Wayland requires 'wl-copy' and 'wl-paste'.
all.equal(x, res)
#> Error: object 'res' not found
x; res
#> [1] "0.316362437326461129"
#> Error: object 'res' not found
```
