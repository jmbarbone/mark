# Get TODOs

Search for ``` #`` TODO ``` tags

## Usage

``` r
todos(
  pattern = NULL,
  path = ".",
  force = getOption("mark.todos.force"),
  ext = getOption("mark.todos.ext"),
  ignore = NULL,
  ...
)

fixmes(
  pattern = NULL,
  path = ".",
  force = getOption("mark.todos.force"),
  ext = getOption("mark.todos.ext"),
  ignore = NULL,
  ...
)
```

## Arguments

- pattern:

  A character string containing a regular expression to filter for
  comments after tags; default `NULL` does not filter

- path:

  Where to search for the todos. If this is a directory, paths matching
  the `ext` will be included. If a file, `ext` is ignored.

- force:

  If `TRUE` will force searching for files in directories that do not
  contain an `.Rproj` file. This can be controlled with the option
  `mark.todos.force`

- ext:

  A vector of file extensions to search for todos. Ignored when `path`
  is not a directory or when `NULL`.

- ignore:

  A regular expression for files to ignore. Ignored if `path` is not a
  directory or when `NULL`.

- ...:

  Additional parameters passed to `grep` (Except for `pattern`, `x`, and
  `value`)

## Value

`NULL` if none are found, otherwise a `data.frame` with the line number,
file name, and TODO comment.

## Details

Searches for `TODO` comments in files. Extensions with `md`, `Rmd`, and
`qmd` specifically search for a `<-- TODO * -->` string, whereas
everything else is found with `# TODO`.

## Examples

``` r
if (FALSE) { # \dontrun{
file <- tempfile()
writeLines(c(
  "# TODO make x longer",
  "x <- 1:10",
  "length(x)",
  "# TODO add another example",
  "# FIXME This is a fixme"
  ), file)
todos(path = file)
todos("example", path = file)
fixmes(path = file)
file.remove(file)
} # }
```
