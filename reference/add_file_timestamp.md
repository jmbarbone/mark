# Add file timestamp

Adds a timestamp to a file

## Usage

``` r
add_file_timestamp(
  x,
  ts = Sys.time(),
  format = "%Y-%m-%d %H%M%S",
  sep = " "
)
```

## Arguments

- x:

  A vector of files

- ts:

  A single timestamp or vector of timestamps (default:
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html))

- format:

  A format to be applied to the times; set to `NULL` to skip formatting

- sep:

  A `character` vector of length 1 to separate the timestamp from the
  file name

## Value

The full name paths with the appended time stamp

## Examples

``` r
file1 <- tempfile(fileext = ".txt")
file2 <- tempfile()

add_file_timestamp(file1)
#> /tmp/RtmpPDZwuu/file1bef788a2639 2026-02-21 224709.txt
add_file_timestamp(file2)
#> /tmp/RtmpPDZwuu/file1bef34bcbab5 2026-02-21 224709

file.remove(file1, file2)
#> Warning: cannot remove file '/tmp/RtmpPDZwuu/file1bef788a2639.txt', reason 'No such file or directory'
#> Warning: cannot remove file '/tmp/RtmpPDZwuu/file1bef34bcbab5', reason 'No such file or directory'
#> [1] FALSE FALSE
```
