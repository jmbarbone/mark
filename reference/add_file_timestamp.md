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
  [`base::Sys.time()`](https://rdrr.io/r/base/Sys.time.html))

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
#> /tmp/Rtmpwzx8p0/file1b5b2b9953c5 2026-06-28 022611.txt
add_file_timestamp(file2)
#> /tmp/Rtmpwzx8p0/file1b5b2d353ce8 2026-06-28 022611
```
