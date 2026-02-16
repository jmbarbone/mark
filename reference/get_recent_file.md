# Get recent file

A function where you can detect the most recent file from a directory.

## Usage

``` r
get_recent_file(x, exclude_temp = TRUE, ...)
```

## Arguments

- x:

  The directory in which to search the file

- exclude_temp:

  Logical, if `TRUE` tries to remove temp Windows files

- ...:

  Additional arguments passed to
  [`list_files()`](https://jmbarbone.github.io/mark/reference/file_utils.md)

## Value

The full name of the most recent file from the stated directory
