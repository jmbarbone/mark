# Write file with md5 hash check

Write file with md5 hash check

## Usage

``` r
write_file_md5(
  x,
  path = NULL,
  method = mark_write_methods(),
  overwrite = NA,
  quiet = FALSE,
  encoding = "UTF-8",
  compression = getOption("mark.compress.method", mark_compress_methods()),
  ...
)

mark_write_methods()

mark_compress_methods()
```

## Arguments

- x:

  An object to write to file

- path:

  The file or connection to write to (dependent on part by method)

- method:

  The method of saving the file. When `default`, the method is
  determined by file extension of `path`, if present, otherwise by the
  type of object of `x`.

- overwrite:

  When `NA`, only saves if the md5 hashes do not match. Otherwise, see
  [`fs::file_copy()`](https://fs.r-lib.org/reference/copy.html).

- quiet:

  When `TRUE`, suppresses messages from md5 checks.

- encoding:

  The encoding to use when writing the file.

- compression:

  The compression method to use when writing the file.

- ...:

  Additional arguments passed to the write function.

## Value

- `write_file_md5()`: `x`, invisibly. When `path` is not the
  [`stdout()`](https://rdrr.io/r/base/showConnections.html), `x` is
  returned with the attribute `"path"` set to the result of
  [`file_copy_md5()`](https://jmbarbone.github.io/mark/reference/file_copy_md5.md).

- `mark_write_methods()`: A list of applicable methods and their aliases

- `mark_compress_methods()`: A character vector of applicable
  compression methods

## [`options()`](https://rdrr.io/r/base/options.html)

- `mark.compress.method`: compression method to use when writing files

- `mark.list.hook`: when a `data.frame` contains a `list` column, this
  function is applied to each element of the list. The default `"auto"`
  uses `toJSON()` if the package `jsonlite` is available, otherwise

## Examples

``` r
# just writes to stdout()
df <- data.frame(a = 1, b = 2)
write_file_md5(df)
#> "a" "b"
#> 1 2

temp <- tempfile()
write_file_md5(df, temp) # new
#> <fileCopyMd5Message> /tmp/RtmpPDZwuu/file1bef2ed5f100 (new file)
#> package:mark
write_file_md5(df, temp) # same
#> <fileCopyMd5Message> /tmp/RtmpPDZwuu/file1bef2ed5f100 (md5 same)
#> package:mark
df$c <- 3
write_file_md5(df, temp) # changes
#> <fileCopyMd5Message> /tmp/RtmpPDZwuu/file1bef2ed5f100 (md5 change)
#> package:mark
fs::file_delete(temp)
```
