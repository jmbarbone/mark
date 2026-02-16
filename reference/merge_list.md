# Merge lists

Merge lists with different or intersecting names

## Usage

``` r
merge_list(x, y, keep = c("x", "y"), null = c("ignore", "drop", "keep")[1:2])
```

## Arguments

- x, y:

  Lists to merge

- keep:

  When matching names are found, from which object should the values be
  retained; `"x"` retains values from `x`, `"y"` retains values from
  `y`.

- null:

  Method for handling `NULL` values. When two values are passed, they
  will be applied to `x` and `y` respectively. When a single value is
  passed, it will be applied to both `x` and `y`.

  - `"ignore"`: `NULL` values are ignored. When passes to `x`, the
    `NULL` values will be retained if they are not overridden by `y`.

  - `"drop"`: `NULL` values are dropped from merge and will not appear
    in the output.

  - `"keep"`: `NULL` values are retained in the output and can override
    other values.

## Examples

``` r
x <- list(a = 1, b = 2,    c = NULL, d = NULL)
y <- list(a = 2, b = NULL, c = 3)

# compared to:
utils::modifyList(x, y)
#> $a
#> [1] 2
#> 
#> $c
#> [1] 3
#> 
#> $d
#> NULL
#> 
utils::modifyList(x, y, keep.null = TRUE)
#> $a
#> [1] 2
#> 
#> $b
#> NULL
#> 
#> $c
#> [1] 3
#> 
#> $d
#> NULL
#> 

merge_list(x, y)
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
#> $c
#> [1] 3
#> 
#> $d
#> NULL
#> 
merge_list(x, y, keep = "y")
#> $a
#> [1] 2
#> 
#> $b
#> [1] 2
#> 
#> $c
#> [1] 3
#> 
#> $d
#> NULL
#> 
merge_list(x, y, null = "drop")
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
#> $c
#> [1] 3
#> 
```
