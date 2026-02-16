# Get and bump version

Will read the `DESCRIPTION` file and to get and adjust the version

`bump_date_version()` will not check if the version is actually a date.
When the current version is the same as today's date(equal by character
strings) it will append a `.1`.

## Usage

``` r
get_version()

bump_version(version = NULL)

bump_date_version(version = NULL)

update_version(version = NULL, date = FALSE)
```

## Arguments

- version:

  A new version to be added; default of `NULL` will automatically
  update.

- date:

  If `TRUE` will use a date as a version.

## Value

- `get_version()`: A package_version

- `bump_version()`: None, called for its side-effects

- `bump_date_version()`: None, called for its side-effects

- `update_version()`: None, called for its side-effects

## Details

Get and bump package version for dates
