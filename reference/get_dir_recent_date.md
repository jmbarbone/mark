# Get recent directory by date

Looks at the directories and assumes the date

## Usage

``` r
get_dir_recent_date(x = ".", dt_pattern = NULL, dt_format = NULL, all = FALSE)
```

## Arguments

- x:

  A directory

- dt_pattern:

  A pattern to be passed to filter for the directory

- dt_format:

  One or more formats to try

- all:

  Logical, if `TRUE` will recursively search for directories

## Value

A full path to a directory
