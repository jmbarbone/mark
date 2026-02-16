# Save source

Source a file and save as file

## Usage

``` r
save_source(env = parent.frame(), file = mark_temp("Rds"), name = NULL)
```

## Arguments

- env:

  The parent environment

- file:

  The file to save the environment to

- name:

  An optional name for the environment (mostly cosmetic)

## Value

A `source_env`/`environment` object, created from `env`
