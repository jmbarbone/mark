# Dataframe labels

Assign labels to a vector or data.frame.

## Usage

``` r
assign_labels(x, ...)

# Default S3 method
assign_labels(x, label, ...)

# S3 method for class 'data.frame'
assign_labels(
  x,
  ...,
  .missing = c("error", "warn", "skip"),
  .ls = rlang::list2(...)
)

get_labels(x)

# Default S3 method
get_labels(x)

# S3 method for class 'data.frame'
get_labels(x)

view_labels(x, title)

remove_labels(x, ...)

# Default S3 method
remove_labels(x, ...)

# S3 method for class 'data.frame'
remove_labels(x, cols, ...)
```

## Arguments

- x:

  A vector of data.frame

- ...:

  One or more unquoted expressed separated by commas. If assigning to a
  data.frame, `...` can be replaced with a `data.frame` where the first
  column is the targeted colname and the second is the desired label.

- label:

  A single length string of a label to be assigned

- .missing:

  A control setting for dealing missing columns in a list; can be set to
  `error` to [`stop()`](https://rdrr.io/r/base/stop.html) the call,
  `warn` to provide a warning, or `skip` to silently skip those labels.

- .ls:

  A named list of columns and labels to be set if `...` is empty

- title:

  Title for the viewer window – if not supplemented will show as
  `paste0(as.character(substitute(x)), " - Labels")`

- cols:

  A character vector of column names; if missing will remove the label
  attribute across all columns

## Value

A labelled vector or `data.frame`

## Details

When labels are assigned to a data.frame they can make viewing the
object (with [`View()`](https://rdrr.io/r/utils/View.html) inside
Rstudio). The `view_labels()` has a call to
[`View()`](https://rdrr.io/r/utils/View.html) inside and will retrieve
the labels and show them in the viewer as a data.frame.

## Examples

``` r
labs <- assign_labels(
  iris,
  Sepal.Length = "cms",
  Sepal.Width  = "cms",
  Petal.Length = "cms",
  Petal.Width  = "cms",
  Species      = "Iris ..."
)

labs$dummy <- ""
get_labels(labs) # shows label as <NA> for dummy column
#>         column    label
#> 1 Sepal.Length      cms
#> 2  Sepal.Width      cms
#> 3 Petal.Length      cms
#> 4  Petal.Width      cms
#> 5      Species Iris ...
#> 6        dummy     <NA>

labs0 <- remove_labels(labs, c("Sepal.Length", "Sepal.Width"))
get_labels(labs0) # No labels for Sepal.Length and Sepal.Width
#>         column    label
#> 1 Sepal.Length     <NA>
#> 2  Sepal.Width     <NA>
#> 3 Petal.Length      cms
#> 4  Petal.Width      cms
#> 5      Species Iris ...
#> 6        dummy     <NA>
```
