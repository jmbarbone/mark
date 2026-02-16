# Is File/Directory

Is the path a file/directory?

## Usage

``` r
is_dir(x)

is_file(x)
```

## Arguments

- x:

  A vector of file paths

## Value

A `logical` vector

## Details

These are essentially taken from
[`utils::file_test()`](https://rdrr.io/r/utils/filetest.html) for
`op = '-d'` and `op = -f` but separated.
