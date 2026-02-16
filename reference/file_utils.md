# Open a file using windows file associations

Opens the given files(s)

## Usage

``` r
open_file(x)

file_open(x)

shell_exec(x)

list_files(
  x = ".",
  pattern = utils::glob2rx(glob),
  glob = NULL,
  ignore_case = FALSE,
  all = FALSE,
  negate = FALSE,
  basename = FALSE
)

list_dirs(
  x = ".",
  pattern = NULL,
  ignore_case = FALSE,
  all = FALSE,
  basename = FALSE,
  negate = FALSE
)
```

## Arguments

- x:

  A character vector of paths

- pattern, glob:

  Pattern to search for files. `glob` is absorbed into `pattern`,
  through [`utils::glob2rx()`](https://rdrr.io/r/utils/glob2rx.html).

- ignore_case:

  logical. Should pattern-matching be case-insensitive?

- all:

  a logical value. If FALSE, only the names of visible files are
  returned (following Unix-style visibility, that is files whose name
  does not start with a dot). If TRUE, all file names will be returned.

- negate:

  Logical, if `TRUE` will inversely select files that do not match the
  provided pattern

- basename:

  If `TRUE` only searches pattern on the basename, otherwise on the
  entire path

## Value

- `open_file()`, `shell_exec()`: A logical vector where `TRUE`
  successfully opened, `FALSE` did not and `NA` did not try to open
  (file not found)

- `list_files()`, `list_dirs()`: A vector of full paths

## Details

`open_file` is an alternative to `shell.exec()` that can take take
multiple files. `list_files` and `list_dirs` are mostly wrappers for
[`fs::dir_ls()`](https://fs.r-lib.org/reference/dir_ls.html) with
preferred defaults and pattern searching on the full file path.

`file_open` is simply an alias.
