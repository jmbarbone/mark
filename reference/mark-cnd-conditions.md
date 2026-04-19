# Conditions for `mark`

Conditions for `mark`

## Details

The following conditions are defined in the `{mark}` package.

## `{cnd}`

These conditions are made with the
[`{cnd}`](https://jmbarbone.github.io/cnd/reference/cnd-package.html)
package though the use of
[`cnd::condition()`](https://jmbarbone.github.io/cnd/reference/condition.html).

## `{mark}` conditions

### `mark:append_warning/warning`

- package:

  `{mark}`

- class:

  `mark:append_warning`

- type:

  **warning**

*no help documentation provided*

### `mark:assign_labels_error/error`

- package:

  `{mark}`

- class:

  `mark:assign_labels_error`

- type:

  **error**

`assign_labels_error`

**Columns not found** You can set `.missing` to `warn` to get a warning
instead of an error, or `skip` to silently skip those labels.

**Malformed labels** If passing labels as `.ls`, `...` must be empty.
Columns in `...` must be entered as `name = value`; `column = label`.

    # instead of this:
    assign_labels(df, a = 'AAA', .ls = list(b = 'BBB'))

    # do this:
    assign_labels(df, a = 'AAA', b = 'BBB')
    # or this:
    assign_labels(df, .ls = list(a = 'AAA', b = 'BBB'))

Labels must not be null; to remove labels, use
[`remove_labels()`](https://jmbarbone.github.io/mark/reference/labels.md).

    # instead of this
    assign_labels(df, a = 'AAA', b = NULL)

    # do this
    df <- assign_labels(df, a = 'AAA')
    df <- remove_labels(df, 'b')

### `mark:base_conversion_error/error`

- package:

  `{mark}`

- class:

  `mark:base_conversion_error`

- type:

  **error**

*no help documentation provided*

### `mark:description_version_error/error`

- package:

  `{mark}`

- class:

  `mark:description_version_error`

- type:

  **error**

*no help documentation provided*

### `mark:fct_expand_seq_error/error`

- package:

  `{mark}`

- class:

  `mark:fct_expand_seq_error`

- type:

  **error**

*no help documentation provided*

### `mark:import_error/error`

- package:

  `{mark}`

- class:

  `mark:import_error`

- type:

  **error**

The object you are trying to import has already been assigned in the
environment you are importing to. Use the `overwrite` option to replace
the object.

For example:

    # instead of
    foo <- NULL
    import('package', 'foo')

    # do this
    foo <- NULL
    import('package', 'foo', overwrite = TRUE)

### `mark:internal_error/error`

- package:

  `{mark}`

- class:

  `mark:internal_error`

- type:

  **error**

Generic error to capture internal `{mark}` errors. If any of these are
encountered, please report an issue at
<https://github.com/jmbarbone/mark/issues>

### `mark:list2df_warning/warning`

- package:

  `{mark}`

- class:

  `mark:list2df_warning`

- type:

  **warning**

*no help documentation provided*

### `mark:match_arg_error/error`

- package:

  `{mark}`

- class:

  `mark:match_arg_error`

- type:

  **error**

*no help documentation provided*

### `mark:match_param_error/error`

- package:

  `{mark}`

- class:

  `mark:match_param_error`

- type:

  **error**

*no help documentation provided*

### `mark:md5_condition/condition`

- package:

  `{mark}`

- class:

  `mark:md5_condition`

- type:

  **condition**

Produces messages on md5 checks when `file_copy_md5(quiet = FALSE)`. The
message will indicate whether the file was new, or if the md5 hash was
the same or different. When `quiet = TRUE`, no messages will be
produced.

### `mark:na_timezone_found_warning/warning`

- package:

  `{mark}`

- class:

  `mark:na_timezone_found_warning`

- type:

  **warning**

*no help documentation provided*

### `mark:not_available_error/error`

- package:

  `{mark}`

- class:

  `mark:not_available_error`

- type:

  **error**

*no help documentation provided*

### `mark:numeric_datetime_tz_error/error`

- package:

  `{mark}`

- class:

  `mark:numeric_datetime_tz_error`

- type:

  **error**

    # Instead of this:
    diff_time(100, 200, tz = NULL)

    # do this:
    diff_time(100, 200, tz = 'America/New_York')

    # or:
    diff_time(100, 200, tz = 0)

### `mark:options_error/error`

- package:

  `{mark}`

- class:

  `mark:options_error`

- type:

  **error**

*no help documentation provided*

### `mark:path_error/error`

- package:

  `{mark}`

- class:

  `mark:path_error`

- type:

  **error**

*no help documentation provided*

### `mark:path_warning/warning`

- package:

  `{mark}`

- class:

  `mark:path_warning`

- type:

  **warning**

File creation cannot be performed when the path is an existing directory

### `mark:reindex_error/error`

- package:

  `{mark}`

- class:

  `mark:reindex_error`

- type:

  **error**

*no help documentation provided*

### `mark:reindex_warning/warning`

- package:

  `{mark}`

- class:

  `mark:reindex_warning`

- type:

  **warning**

NA values in index may cause errors with expansion

[`reindex()`](https://jmbarbone.github.io/mark/reference/reindex.md)
will not match on NA values but instead will return a row of NAs

    x <- data.frame(index = c(1:2, NA, 4:5), values = letters[1:5])
    reindex(x, 'index', c(1, 2, 5))
    #>   index values
    #> 1     1      a
    #> 2     2      b
    #> 5     5      e

    reindex(x, 'index', c(1, 2, 5, NA))
    #>      index values
    #> 1        1      a
    #> 2        2      b
    #> 5        5      e
    #> <NA>    NA   <NA>

### `mark:source_error/error`

- package:

  `{mark}`

- class:

  `mark:source_error`

- type:

  **error**

*no help documentation provided*

### `mark:switch_error/condition`

- package:

  `{mark}`

- class:

  `mark:switch_error`

- type:

  **condition**

*no help documentation provided*

### `mark:timezone_not_found_error/error`

- package:

  `{mark}`

- class:

  `mark:timezone_not_found_error`

- type:

  **error**

When using a string for a timezone, this value must be found within
[`OlsonNames()`](https://rdrr.io/r/base/timezones.html)

### `mark:view_labels_error/error`

- package:

  `{mark}`

- class:

  `mark:view_labels_error`

- type:

  **error**

This may be because you are using RStudio :
https://community.rstudio.com/t/view-is-causing-an-error/75297/4 You can
try : `utils::View(get_labels(x), title = "Labels")`

## See also

[cnd::cnd-package](https://jmbarbone.github.io/cnd/reference/cnd-package.html)
[cnd::condition](https://jmbarbone.github.io/cnd/reference/condition.html)
