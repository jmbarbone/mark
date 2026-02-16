# List all environments and objects

Functions to list out all environments and objects

## Usage

``` r
environments()

ls_all(all.names = FALSE)

objects_all(all.names = FALSE)
```

## Arguments

- all.names:

  a logical value. If `TRUE`, all object names are returned. If `FALSE`,
  names which begin with a `.` are omitted.

## Value

- `environments()`: Invisibly, a `character` vector of environment names

&nbsp;

- `ls_all()`, `objects_all()`: A named list for each of the environments
  the [`search()`](https://rdrr.io/r/base/search.html) path with all the
  objects found in that environment

## Details

`environments()` is basically a printing wrapper for
[`base::search()`](https://rdrr.io/r/base/search.html)

`ls_all()` and `objects_all()` can be used retrieved all objects from
all environments in the [`search()`](https://rdrr.io/r/base/search.html)
path, which may print out a large result into the console.
