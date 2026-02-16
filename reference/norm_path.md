# Normalize paths

Normalize and check a vector of paths

## Usage

``` r
norm_path(x = ".", check = FALSE, remove = check)

file_path(..., check = FALSE, remove = check)

user_file(..., check = FALSE, remove = check)
```

## Arguments

- x:

  A character vector of paths

- check:

  Logical, if TRUE will check if the path exists and output a warning if
  it does not.

- remove:

  Logical, if TRUE will remove paths that are not found

- ...:

  Character vectors for creating a path

## Value

A vector of full file paths
