# List Objects - extensions

List Objects - extensions

## Usage

``` r
ls_dataframe(pattern, all.names = FALSE, envir = parent.frame())

ls_function(pattern, all.names = FALSE, envir = parent.frame())

ls_object(pattern, all.names = FALSE, envir = parent.frame())
```

## Arguments

- pattern:

  an optional [regular expression](https://rdrr.io/r/base/regex.html).
  Only names matching `pattern` are returned.
  [`glob2rx`](https://rdrr.io/r/utils/glob2rx.html) can be used to
  convert wildcard patterns to regular expressions.

- all.names:

  a logical value. If `TRUE`, all object names are returned. If `FALSE`,
  names which begin with a `.` are omitted.

- envir:

  an alternative argument to `name` for specifying the environment.
  Mostly there for back compatibility.

## Value

A `character` vector of names
