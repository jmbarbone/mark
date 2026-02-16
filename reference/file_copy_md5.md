# File copy with md5 hash check

File copy with md5 hash check

## Usage

``` r
file_copy_md5(path, new_path, overwrite = NA, quiet = FALSE)
```

## Arguments

- path:

  A character vector of one or more paths.

- new_path:

  A character vector of paths to the new locations.

- overwrite:

  When `NA`, only saves if the md5 hashes do not match. Otherwise, see
  [`fs::file_copy()`](https://fs.r-lib.org/reference/copy.html).

- quiet:

  When `TRUE`, suppresses messages from md5 checks.

## Value

The path(s) of the new file(s), invisibly. When `overwrite` is `NA`, the
paths will be returned with two addition attributes, `"changed"`, a
logical vector indicating whether the file was changed (`NA` for when
the file is new), and `"md5sum"`, a list of the md5sums of the old and
new md5 sums.
