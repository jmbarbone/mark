# Changelog

## mark (development version)

- [`make_sf()`](https://jmbarbone.github.io/mark/reference/make_sf.md)
  now uses [`substitute()`](https://rdrr.io/r/base/substitute.html) to
  provide more information inside the function body when viewing
  [\#129](https://github.com/jmbarbone/mark/issues/129)
- [`round_to()`](https://jmbarbone.github.io/mark/reference/round_to.md)
  added to round values to a set
  [\#142](https://github.com/jmbarbone/mark/issues/142)
- [`is_true()`](https://jmbarbone.github.io/mark/reference/logic_ext.md),
  [`is_false()`](https://jmbarbone.github.io/mark/reference/logic_ext.md)
  now works as documented
  [\#262](https://github.com/jmbarbone/mark/issues/262)
- [`to_boolean()`](https://jmbarbone.github.io/mark/reference/to_boolean.md)
  now works as documented
  [\#262](https://github.com/jmbarbone/mark/issues/262)
  - [`to_boolean.character()`](https://jmbarbone.github.io/mark/reference/to_boolean.md),
    and
    [`to_boolean.factor()`](https://jmbarbone.github.io/mark/reference/to_boolean.md)
    have been improved; they will also now require exact matches;
    cleanup (e.g., trimming whitespace and lowercasing) are not longer
    performed
  - [`to_boolean.integer()`](https://jmbarbone.github.io/mark/reference/to_boolean.md)
    added
  - [`to_boolean.numeric()`](https://jmbarbone.github.io/mark/reference/to_boolean.md),
    [`to_boolean.integer()`](https://jmbarbone.github.io/mark/reference/to_boolean.md)
    will now return `NA`
- `md5(bytes)` added to use `tools::md5sum(bytpes)` for **R \> 4.2.0**
  [\#258](https://github.com/jmbarbone/mark/issues/258)
- [`md5()`](https://jmbarbone.github.io/mark/reference/md5.md) now uses
  little-endian serialization (*i.e.*, `serialize(xdr = FALSE)`) for
  more consistent results across platforms and faster speed; which may
  cause hashes created prior to *change*

## mark 0.8.3

CRAN release: 2025-04-23

- [`file_copy_md5()`](https://jmbarbone.github.io/mark/reference/file_copy_md5.md)
  now produces more messages
  [\#239](https://github.com/jmbarbone/mark/issues/239)
- [`file_copy_md5()`](https://jmbarbone.github.io/mark/reference/file_copy_md5.md)
  saves md5 sum checks as an attribute
- removes tests for
  [`struct()`](https://jmbarbone.github.io/fuj/reference/struct.html)
  [\#252](https://github.com/jmbarbone/mark/issues/252)

## mark 0.8.2

CRAN release: 2024-12-07

- [`read_clipboard()`](https://jmbarbone.github.io/mark/reference/clipboard.md)
  and
  [`write_clipboard()`](https://jmbarbone.github.io/mark/reference/clipboard.md)
  now use [clipr](https://github.com/mdlincoln/clipr) to work on
  non-Windows platforms
  [\#125](https://github.com/jmbarbone/mark/issues/125)
- [`read_clipboard()`](https://jmbarbone.github.io/mark/reference/clipboard.md)
  now works with more methods for reading `data.frame`s
- [`read_clipboard()`](https://jmbarbone.github.io/mark/reference/clipboard.md)
  now defaults to a `tibble` return when
  [tibble](https://tibble.tidyverse.org/) is available
- timezone testing updated
  [\#247](https://github.com/jmbarbone/mark/issues/247)
- actions updated

## mark 0.8.1

CRAN release: 2024-09-29

- [`write_file_md5()`](https://jmbarbone.github.io/mark/reference/write_file_md5.md)
  now supports `"feather"` and `"parquet"` methods as wrappers for
  [`{arrow}`](https://arrow.apache.org/docs/r/)
  [\#234](https://github.com/jmbarbone/mark/issues/234)
- [`md5()`](https://jmbarbone.github.io/mark/reference/md5.md) added to
  provide MD5 check sums for objects
  [\#233](https://github.com/jmbarbone/mark/issues/233)
- [`unique_rows()`](https://jmbarbone.github.io/mark/reference/unique_rows.md)
  added to subset on (non-)duplicated rows in a `data.frame`
  [\#87](https://github.com/jmbarbone/mark/issues/87)
- [`within()`](https://jmbarbone.github.io/mark/reference/within.md)
  added as an alternative to
  [`between_more()`](https://jmbarbone.github.io/mark/reference/within.md)
  [\#120](https://github.com/jmbarbone/mark/issues/120)
- test updated for upcoming R release
  [\#240](https://github.com/jmbarbone/mark/issues/240)

## mark 0.8.0

CRAN release: 2024-05-23

### breaking changes

- `echo()` is now removed; use
  [`echo::echo()`](https://jmbarbone.github.io/echo/reference/echo.html)
  instead [\#214](https://github.com/jmbarbone/mark/issues/214)
- includes
  [`tryn()`](https://jmbarbone.github.io/mark/reference/tryn.md) for
  running an expression a maximum number of times before failure
  [\#80](https://github.com/jmbarbone/mark/issues/80)

### fixes

- [`unlist0()`](https://jmbarbone.github.io/mark/reference/unlist0.md)
  no longer fails when input list is not named
  [\#220](https://github.com/jmbarbone/mark/issues/220)

### improvements

- [`match_param()`](https://jmbarbone.github.io/mark/reference/match_param.md)
  has been improved
  - can now return multiple matches
    [\#191](https://github.com/jmbarbone/mark/issues/191), and can
    return partial matches
  - error message readability improved for
    `matchParamMatchError`[\#194](https://github.com/jmbarbone/mark/issues/194)
  - `choices` can now be a list of `formula` elements, preserving the
    return value

### new features

- [`file_copy_md5()`](https://jmbarbone.github.io/mark/reference/file_copy_md5.md)
  added as a wrapper for
  [`fs::file_copy()`](https://fs.r-lib.org/reference/copy.html) but
  provides MD5 checks through
  [`tools::md5sum()`](https://rdrr.io/r/tools/md5sum.html) to avoid
  overwriting files that had no content changes
  [\#207](https://github.com/jmbarbone/mark/issues/207)
- [`write_file_md5()`](https://jmbarbone.github.io/mark/reference/write_file_md5.md)
  added as a general writing function and utilizes
  [`file_copy_md5()`](https://jmbarbone.github.io/mark/reference/file_copy_md5.md)
  for MD5 checks (including some compression options)
  [\#207](https://github.com/jmbarbone/mark/issues/207),
  [\#224](https://github.com/jmbarbone/mark/issues/224)

## mark 0.7.0

CRAN release: 2023-10-23

- [`merge_list()`](https://jmbarbone.github.io/mark/reference/merge_list.md)
  added for combining lists
  [\#200](https://github.com/jmbarbone/mark/issues/200)
- [`glob()`](https://jmbarbone.github.io/mark/reference/glob.md) added
  for basic wildcard globbing on character vectors
  [\#167](https://github.com/jmbarbone/mark/issues/167)
- adds greater use of [fs](https://fs.r-lib.org) over base file
  functions [\#160](https://github.com/jmbarbone/mark/issues/160)
- improvements in
  [`todos()`](https://jmbarbone.github.io/mark/reference/todos.md) and
  [`fixmes()`](https://jmbarbone.github.io/mark/reference/todos.md)
  - File extension can now be set
    [\#170](https://github.com/jmbarbone/mark/issues/170), which by
    default includes `qmd`
    ([\#163](https://github.com/jmbarbone/mark/issues/163)) and `py`
    files
  - new parameter `ignore` to ignore any files
  - file paths and line numbers can now be *clicked* within RStudio
    [\#171](https://github.com/jmbarbone/mark/issues/171)
- adds more use of
  [`rlang::list2()`](https://rlang.r-lib.org/reference/list2.html) for
  internally [\#199](https://github.com/jmbarbone/mark/issues/199)
- GitHub action included to check version updates with pull requests
  [\#211](https://github.com/jmbarbone/mark/issues/211)
- `%::%` and `%:::%` now exported from
  [fuj](https://jmbarbone.github.io/fuj/)

## mark 0.6.1

CRAN release: 2023-09-18

- updates Timezone references for upcoming R release
  [\#203](https://github.com/jmbarbone/mark/issues/203)

## mark 0.6.0

CRAN release: 2023-05-06

### New features

- new functions for detecting *blank* values in a vector or
  `data.frame`. *Blank* values are those which do not contain any text
  (controls for `NA`) or are entirely white space.
  - [`is_blank()`](https://jmbarbone.github.io/mark/reference/blank_values.md)
    for detecting *blank* values in a vector
  - [`is_blank_cols()`](https://jmbarbone.github.io/mark/reference/blank_values.md)
    for detecting *blank* columns
  - [`select_blank_cols()`](https://jmbarbone.github.io/mark/reference/blank_values.md)
    for selecting *blank* columns
  - [`remove_blank_cols()`](https://jmbarbone.github.io/mark/reference/blank_values.md)
    for removing *blank* columns
- [`match_param()`](https://jmbarbone.github.io/mark/reference/match_param.md)
  now accepts a named listed for alias matching
  [\#104](https://github.com/jmbarbone/mark/issues/104)
- `echo()` evaluates expressions and logs outputs
  [\#164](https://github.com/jmbarbone/mark/issues/164)
- [fuj](https://jmbarbone.github.io/fuj/) is now imported
  - multiple functions now re-exported from
    [fuj](https://jmbarbone.github.io/fuj/) (see `?mark::reexports`)
  - [`set_names0()`](https://jmbarbone.github.io/mark/reference/set_names0.md)
    is deprecated in favor of
    [`set_names()`](https://jmbarbone.github.io/fuj/reference/names.html)
  - error messages are created with
    [`fuj::new_condition()`](https://jmbarbone.github.io/fuj/reference/new_condition.html);
  - test for errors and warnings enhanced with class checks

### Fixes and updates

- [`date_from_partial()`](https://jmbarbone.github.io/mark/reference/date_from_partial.md)
  works again [\#155](https://github.com/jmbarbone/mark/issues/155)
  after fixing an issue with an internal utility
  `is_valid_date_string()` that wasn’t recognizing `%Y-%m-%d` (and
  potentially others)
- `lintr` GitHub action updated
  [\#173](https://github.com/jmbarbone/mark/issues/173); this includes
  plenty of internal improvements and code cleanup
- package description in help files corrected
  [\#165](https://github.com/jmbarbone/mark/issues/165)
- GitHub pages updated with latest [pkgdown](https://pkgdown.r-lib.org/)
  action [\#175](https://github.com/jmbarbone/mark/issues/175)
- Update to GitHub R-CMD-check action
  [\#178](https://github.com/jmbarbone/mark/issues/178)
- [`switch_in_case()`](https://jmbarbone.github.io/mark/reference/switch-ext.md)
  handles `NA`s better
  [\#183](https://github.com/jmbarbone/mark/issues/183)
- internal `switch` tests updated for [waldo](https://waldo.r-lib.org)
  development [\#182](https://github.com/jmbarbone/mark/pulls/182)
  thanks, `@hadley`
- methods for
  [`write_clipboard()`](https://jmbarbone.github.io/mark/reference/clipboard.md)
  are now displayed in documentation
  [\#186](https://github.com/jmbarbone/mark/issues/182)

## mark 0.5.3

CRAN release: 2022-10-16

- CRAN fix for new release
  [\#151](https://github.com/jmbarbone/mark/issues/151)

## mark 0.5.2

CRAN release: 2022-10-01

### New features

- [`normalize()`](https://jmbarbone.github.io/mark/reference/normalize.md)
  added to normalize values in `vectors`, `matrices`, and `data.frame`s
  by specified ranges and boundaries
  [\#143](https://github.com/jmbarbone/mark/issues/143)
- [`get_labels()`](https://jmbarbone.github.io/mark/reference/labels.md)
  and other label related functions now get exact matches for `"label"`
  attributes [\#141](https://github.com/jmbarbone/mark/issues/141)
- [`recode_only()`](https://jmbarbone.github.io/mark/reference/recode_by.md),
  and
  [`recode_by()`](https://jmbarbone.github.io/mark/reference/recode_by.md)
  now accept a named [`list()`](https://rdrr.io/r/base/list.html) for
  `by` [\#96](https://github.com/jmbarbone/mark/issues/96)\]
- [`switch_in_case()`](https://jmbarbone.github.io/mark/reference/switch-ext.md)
  now handles functions in the right hand statements
- [`update_version()`](https://jmbarbone.github.io/mark/reference/get_version.md)
  now correctly checks result of embedded
  [`utils::menu()`](https://rdrr.io/r/utils/menu.html) call for updating
  the version [\#123](https://github.com/jmbarbone/mark/issues/121)
- [`require_namespace()`](https://jmbarbone.github.io/fuj/reference/require_namespace.html)
  now accepts multiple namespaces
  [\#121](https://github.com/jmbarbone/mark/issues/121)
- `unique.fact()` S3 method
  [\#86](https://github.com/jmbarbone/mark/issues/86)
- [`recode_only()`](https://jmbarbone.github.io/mark/reference/recode_by.md)
  and
  [`recode_by()`](https://jmbarbone.github.io/mark/reference/recode_by.md)
  can accept a single value for `val`
  [\#73](https://github.com/jmbarbone/mark/issues/73)
- [`fact_reverse()`](https://jmbarbone.github.io/mark/reference/fact_reverse.md)
  for reversing `fact` levels
  [\#78](https://github.com/jmbarbone/mark/issues/78)
- `as.Date.fact()` added
  [\#108](https://github.com/jmbarbone/mark/issues/108)
- `as.character.fact()` added
- `[.fact` added
- [`read_bib()`](https://jmbarbone.github.io/mark/reference/read_bib.md)
  better handles fields where `=` is present in the text
  [\#117](https://github.com/jmbarbone/mark/issues/117)

### Fixes

- [`fact.haven_labelled()`](https://jmbarbone.github.io/mark/reference/fact.md)
  works properly and retains the `label` attribute
  [\#136](https://github.com/jmbarbone/mark/issues/136)
- [`drop_levels()`](https://jmbarbone.github.io/mark/reference/drop_levels.md)
  is exported [\#105](https://github.com/jmbarbone/mark/issues/105)
- [`recode_by()`](https://jmbarbone.github.io/mark/reference/recode_by.md)
  and
  [`recode_only()`](https://jmbarbone.github.io/mark/reference/recode_by.md)
  handle factors better
  [\#81](https://github.com/jmbarbone/mark/issues/81)
- Functions that made use of `shell.exec()` now try to determine the
  appropriate method of opening a file base on OS.
  [\#126](https://github.com/jmbarbone/mark/issues/126)
- Internal functions for potentially coercing factor levels into dates
  no longer try to check for `"%Z"` in the date format
  [\#147](https://github.com/jmbarbone/mark/issues/147)

### Breaking changes

- `reverse()` has been removed (use
  [`flip()`](https://jmbarbone.github.io/fuj/reference/flip.html)
  instead)
- `assign_label()` has been removed (use
  [`assign_labels()`](https://jmbarbone.github.io/mark/reference/labels.md)
  instead)
- `percentile_rank(times)` is deprecated in favor of
  `percent_rank(weights)`

### Non visible changes

- `print.fact()` rewritten as a slightly modified
  [`print.factor()`](https://rdrr.io/r/base/print.html)
  [\#109](https://github.com/jmbarbone/mark/issues/109)
- [`percentile_rank()`](https://jmbarbone.github.io/mark/reference/percentile_rank.md)
  improvements [\#131](https://github.com/jmbarbone/mark/issues/131)

## mark 0.5.1

CRAN release: 2022-08-06

- Fix for CRAN check
  [\#128](https://github.com/jmbarbone/mark/issues/128)

## mark 0.5.0

CRAN release: 2022-03-09

The package website <https://jmbarbone.github.io/mark/> is now
available! More references and vignettes will make their way here in
future releases.

### Fixes

- `detail(NA)` (or when `x` has only `NA` values) no longer throws a
  warning and returns `NA` for `min_c`, `max_c`
  [\#59](https://github.com/jmbarbone/mark/issues/59)
- `print.noted()` now passes `...` to next methods
  [\#67](https://github.com/jmbarbone/mark/issues/67)
- corrects deprecation warning in `assign_label()`
  [\#74](https://github.com/jmbarbone/mark/issues/74)
  - `assign_label()` will be removed in `0.4.2`
- [`set_not_available()`](https://jmbarbone.github.io/mark/reference/not_available.md)
  now seems to work correctly – it probably hasn’t actually be working
  most of the time
- [`percentile_rank()`](https://jmbarbone.github.io/mark/reference/percentile_rank.md)
  is now more correct when `x` is a decimal by checking for unique
  values first [\#92](https://github.com/jmbarbone/mark/issues/92)
- [`counts.data.frame()`](https://jmbarbone.github.io/mark/reference/counts.md)
  now handle factor columns better

### New features

- Functions in
  [`?handlers`](https://jmbarbone.github.io/mark/reference/handlers.md),
  all allow for additional params passed through `...`
  [\#34](https://github.com/jmbarbone/mark/issues/34)
- adds
  [`row_bind()`](https://jmbarbone.github.io/mark/reference/row_bind.md)
  to bind a list of `data.frames()`
  [\#46](https://github.com/jmbarbone/mark/issues/46)
- adds
  [`drop_levels()`](https://jmbarbone.github.io/mark/reference/drop_levels.md)
  with `factor` and `data.frame` methods; functions similarly to
  [`base::droplevels()`](https://rdrr.io/r/base/droplevels.html) but is
  a little faster [\#54](https://github.com/jmbarbone/mark/issues/54)
- [`todos()`](https://jmbarbone.github.io/mark/reference/todos.md) and
  [`fixmes()`](https://jmbarbone.github.io/mark/reference/todos.md) gain
  a new param `force`
  - When `TRUE`, forces searches for `.R` files when the given directory
    does not contain an `.Rproj` file
  - When `FALSE`, prevents long start ups when these functions are
    called in a `.Rprofile` file and R is not launches in a project
    directory
  - This be toggled with a new options `mark.todos.force`
- adds
  [`set_note()`](https://jmbarbone.github.io/mark/reference/note.md), a
  wrapper for `note<-()`
  [\#77](https://github.com/jmbarbone/mark/pull/77)
- adds
  [`fact2char()`](https://jmbarbone.github.io/mark/reference/fact2char.md)
  to compliment
  [`char2fact()`](https://jmbarbone.github.io/mark/reference/char2fact.md)
  [\#75](https://github.com/jmbarbone/mark/pull/75)
- [`print.pseudo_id()`](https://jmbarbone.github.io/mark/reference/print.pseudo_id.md)
  now truncates long uniques to a single line
  [\#70](https://github.com/jmbarbone/mark/pull/70)
- `match_param(NULL, null = TRUE)` allows `param` to safely return
  `NULL` [\#89](https://github.com/jmbarbone/mark/issues/89)
- [`fact_na()`](https://jmbarbone.github.io/mark/reference/fact_na.md)
  is added to use `fact` vectors with `NA` levels that work with
  [`is.na()`](https://rdrr.io/r/base/NA.html)
  [\#69](https://github.com/jmbarbone/mark/issues/69) and other `NA`
  handling improvements
- adds a new `print._mark_bib_df()` method to supporting printing lists
- adds new methods for `facts`: `as.integer.fact()`, `as.double.fact()`,
  [`remove_na.fact()`](https://jmbarbone.github.io/mark/reference/remove_na.md)

### Breaking changes

- [`fact.numeric()`](https://jmbarbone.github.io/mark/reference/fact.md)
  now treats `NaN` the same as `NA`, no extra level/unique value is
  retained
- [`read_clipboard()`](https://jmbarbone.github.io/mark/reference/clipboard.md)
  now returns `NA` when the clipboard is empty, rather than `""`
  (improvements with internal type conversions)
- improvements to `NA` handling as well

### Other, non-visible

- github actions updated
- internal type conversion now heavily relies on
  [`utils::type.convert()`](https://rdrr.io/r/utils/type.convert.html)
  with some additional functionality for logical (e.g., character string
  using `"true"` and `"false"`) and for guessing dates in a `YYYY-MM-DD`
  format
- general clean up and formatting

## mark 0.4.1

CRAN release: 2021-11-05

- `details(factor)` no longer adds `fact` class to `factors`
  [\#50](https://github.com/jmbarbone/mark/issues/50)
- `details()` gains new argument `factor_n` to control threshold for
  making character vectors into factors
- [`detail.data.frame()`](https://jmbarbone.github.io/mark/reference/detail.md)
  now works with single column data.frames
  [\#48](https://github.com/jmbarbone/mark/issues/48)
- [`paste_combine()`](https://jmbarbone.github.io/mark/reference/utils-paste.md)
  no longer duplicated the second vector of `...` when `length(...) > 2`
  [\#52](https://github.com/jmbarbone/mark/issues/52)

## mark 0.4.0

CRAN release: 2021-10-22

### New features

- adds
  [`percentile_rank()`](https://jmbarbone.github.io/mark/reference/percentile_rank.md)
  to calculate percentile ranks with a vector
- adds
  [`insert()`](https://jmbarbone.github.io/mark/reference/insert.md) to
  insert multiple values into a vector
- [`pseudo_id()`](https://jmbarbone.github.io/mark/reference/pseudo_id.md)
  gains argument `na_last` to change positioning of `NA` values
- [`is_true()`](https://jmbarbone.github.io/mark/reference/logic_ext.md)
  and
  [`is_false()`](https://jmbarbone.github.io/mark/reference/logic_ext.md)
  are now exported as generics with methods for `default` and `logical`
- adds
  [`omit_na()`](https://jmbarbone.github.io/mark/reference/omit_na.md)
  for tracking positions of `NA` and non-`NA` values
- `quick_df(NULL)` now returns an empty `data.frame`
- [`quick_dfl()`](https://jmbarbone.github.io/fuj/reference/quick_df.html)
  exported as a wrapper for `quick_df(list(...))`

### Fixes

- [`squash_vec()`](https://jmbarbone.github.io/mark/reference/unlist0.md)
  now works correctly when values are not ordered
  [\#43](https://github.com/jmbarbone/mark/issues/43)
- [`as_ordered()`](https://jmbarbone.github.io/mark/reference/as_ordered.md)
  no longer duplicates `ordered` class
  [\#44](https://github.com/jmbarbone/mark/issues/44)
- [`counts.data.frame()`](https://jmbarbone.github.io/mark/reference/counts.md)
  and
  [`props.data.frame()`](https://jmbarbone.github.io/mark/reference/counts.md)
  correctly make column names unique
- internal `try_numeric()` correctly handles `NA`s
- `flip.matrix(, keep_rownames = FALSE)` now works correctly
- [`any_match()`](https://jmbarbone.github.io/fuj/reference/match_ext.html)
  now works as expected
- [`lines_of_r_code()`](https://jmbarbone.github.io/mark/reference/lines_of_r_code.md)
  now works correctly reading a single file
- `import(, overwrite = TRUE)` now works
- [`ls_function()`](https://jmbarbone.github.io/mark/reference/ls_ext.md),
  [`ls_object()`](https://jmbarbone.github.io/mark/reference/ls_ext.md),
  [`ls_dataframe()`](https://jmbarbone.github.io/mark/reference/ls_ext.md),
  and
  [`ls_all()`](https://jmbarbone.github.io/mark/reference/list_environments.md)
  have improvements for environmental searching
- `assign_labels.data.frame(.missing = "warning")` correctly removes
  missing columns
- [`remove_na.factor()`](https://jmbarbone.github.io/mark/reference/remove_na.md)
  no long drops additional classes other than `ordered` and `factor`

### Others

- documentation of
  [`struct()`](https://jmbarbone.github.io/fuj/reference/struct.html)
  overwriting attributes improved and examples
- adds more unit tests

## mark 0.3.0

CRAN release: 2021-09-18

### Fixes

- [`fact.haven_labelled()`](https://jmbarbone.github.io/mark/reference/fact.md)
  now returns an object with class `fact`
  [\#39](https://github.com/jmbarbone/mark/issues/39); performance
  enhancements
- `set_names0(NULL)` no longer causes an error and returns `NULL`
  [\#40](https://github.com/jmbarbone/mark/issues/40)
- [`diff_time()`](https://jmbarbone.github.io/mark/reference/diff_time.md)
  correctly handles time zones when `x` is `Date` and `y` is `POSIXt`
  [\#41](https://github.com/jmbarbone/mark/issues/41)

### Changes

- updates file path finding functions (e.g.,
  [`list_files()`](https://jmbarbone.github.io/mark/reference/file_utils.md))
  to try to not search every file depending on desired searches (e.g.,
  by full file paths or just base names)
- [`as_ordered()`](https://jmbarbone.github.io/mark/reference/as_ordered.md)
  handles `factors` better; S3 methods removed: `as_ordered.ordered()`,
  `as_ordered.factor()`
- [`remove_na()`](https://jmbarbone.github.io/mark/reference/remove_na.md)
  has better performance when `x` has no `NA` values
- [`counts.data.frame()`](https://jmbarbone.github.io/mark/reference/counts.md)
  and
  [`props.data.frame()`](https://jmbarbone.github.io/mark/reference/counts.md)
  retain attributes of selected columns
- [`todos()`](https://jmbarbone.github.io/mark/reference/todos.md) and
  [`fixmes()`](https://jmbarbone.github.io/mark/reference/todos.md) will
  not search for `.R` or `.Rmd` files if the `path` is not changed from
  `""` and no `.Rproj` is found within the directory

### New features

- adds
  [`unlist0()`](https://jmbarbone.github.io/mark/reference/unlist0.md)
  to retain original names of lists
- adds `%names%` for a fun way to set names
- adds
  [`file_open()`](https://jmbarbone.github.io/mark/reference/file_utils.md)
  as alias for
  [`open_file()`](https://jmbarbone.github.io/mark/reference/file_utils.md)
- adds
  [`detail()`](https://jmbarbone.github.io/mark/reference/detail.md) to
  return a `data.frame` of details for a vector of columns of a
  `data.frame`
- adds
  [`squash_vec()`](https://jmbarbone.github.io/mark/reference/unlist0.md)
  to combined the names of a vector with repeated values
- adds
  [`make_sf()`](https://jmbarbone.github.io/mark/reference/make_sf.md)
  as a simple wrapper for package specific
  [`system.file()`](https://rdrr.io/r/base/system.file.html)
- `add_file_timestampe()` gains a new parameter `sep` to separate the
  file name (sans ext) and the time stamp
- [`assign_labels.data.frame()`](https://jmbarbone.github.io/mark/reference/labels.md)
  gains new argument `.ls` to explicitly set a `list` (or `data.frame`)
  of columns
- [`props()`](https://jmbarbone.github.io/mark/reference/counts.md) and
  family gain argument `na.rm` to not count `NA` values for proportions

## mark 0.2.0

CRAN release: 2021-08-23

### Changes

- [`package_available()`](https://jmbarbone.github.io/mark/reference/package_available.md)
  now visibly returns `TRUE`/`FALSE`
- [`remove_na()`](https://jmbarbone.github.io/mark/reference/remove_na.md)
  now has methods for `list`s and `factor`s
- [`environments()`](https://jmbarbone.github.io/mark/reference/list_environments.md)
  now has it’s own `print.mark_environments()` method rather than
  calling [`cat()`](https://rdrr.io/r/base/cat.html) within the function
  itself
- [`array_extract()`](https://jmbarbone.github.io/mark/reference/array_extract.md)’s
  first argument is changed from `arr` to `.arr`
- [`diff_time()`](https://jmbarbone.github.io/mark/reference/diff_time.md)
  now defaults to using UTC (Related to
  [\#32](https://github.com/jmbarbone/mark/issues/32))
- `print.note()` method has been updated (Related to:
  [\#33](https://github.com/jmbarbone/mark/issues/33)):
  - to print `x` *normally*, without the `note class` when just the note
    has to be seen
  - an internal function now handles the note formatted for class
    `noted`
- changes to
  [`fact()`](https://jmbarbone.github.io/mark/reference/fact.md)
  - [`fact()`](https://jmbarbone.github.io/mark/reference/fact.md) now
    returns a vector with a `fact` element
  - [`fact.character()`](https://jmbarbone.github.io/mark/reference/fact.md)
    correctly labels `NA`s
    [\#24](https://github.com/jmbarbone/mark/issues/24)
  - [`fact.factor()`](https://jmbarbone.github.io/mark/reference/fact.md)
    not longer simply returns `x` but rather updates the levels and
    integer values to confirm with other
    [`fact()`](https://jmbarbone.github.io/mark/reference/fact.md)
    methods.
    [`fact.factor()`](https://jmbarbone.github.io/mark/reference/fact.md)
    will retain all levels of the original value, reorder the levels,
    and append `NA` if necessary
  - [`fact.fact()`](https://jmbarbone.github.io/mark/reference/fact.md)
    added to return a correctly formatted
    [`fact()`](https://jmbarbone.github.io/mark/reference/fact.md)
  - [`fact.logical()`](https://jmbarbone.github.io/mark/reference/fact.md)
    now orders levels as `TRUE` then `FALSE`, and `NA` if present
  - [`fact.Date()`](https://jmbarbone.github.io/mark/reference/fact.md)
    and
    [`fact.POSIXt()`](https://jmbarbone.github.io/mark/reference/fact.md)
    added, which simply call
    [`fact.numeric()`](https://jmbarbone.github.io/mark/reference/fact.md)
  - `print.fact()` method added to print a `fact` vector as a `factor`
  - `as_ordered.factor()` and `as_ordered.ordered()` now call
    [`fact()`](https://jmbarbone.github.io/mark/reference/fact.md) to
    check levels

### Fixes

- functions that check if an argument is a vector no long use
  [`is.vector()`](https://rdrr.io/r/base/vector.html) directly;
  arguments passed with attributes that when removed fulfill
  [`is.vector()`](https://rdrr.io/r/base/vector.html) are accepted
- [`todos()`](https://jmbarbone.github.io/mark/reference/todos.md) and
  [`fixmes()`](https://jmbarbone.github.io/mark/reference/todos.md) now
  correctly show tags for `.Rmd` files
- correction to error message in
  [`limit()`](https://jmbarbone.github.io/mark/reference/limit.md)
- adds missing `sort` argument to
  [`props()`](https://jmbarbone.github.io/mark/reference/counts.md)
- [`pseudo_id.factor()`](https://jmbarbone.github.io/mark/reference/pseudo_id.md)
  no longer returns `NA_integer` when a value is `NA` or a level is `NA`
  and correctly resets the order of the levels from the factor to their
  order of appearance
- `flip.data.frame()` no longer coerces single column data.frames
  [\#36](https://github.com/jmbarbone/mark/issues/36)

### New features

- [`fact.pseudo_id()`](https://jmbarbone.github.io/mark/reference/fact.md)
  and
  [`pseudo_id.pseudo_id()`](https://jmbarbone.github.io/mark/reference/pseudo_id.md)
  methods added
- adds
  [`as_ordered()`](https://jmbarbone.github.io/mark/reference/as_ordered.md)
  to quickly create `ordered` factors using
  [`fact()`](https://jmbarbone.github.io/mark/reference/fact.md)
- adds
  [`char2fact()`](https://jmbarbone.github.io/mark/reference/char2fact.md)
  to convert `character` vectors (or columns in a `data.frame`) to
  `factors` based on the number unique values
- adds
  [`tableNA()`](https://jmbarbone.github.io/mark/reference/tableNA.md)
  to make a table from `NA` values
- [`round_by()`](https://jmbarbone.github.io/mark/reference/round_by.md)
  gains an additional argument `include0` which if `FALSE` will replace
  `0` values with `by`
- [`assign_labels.data.frame()`](https://jmbarbone.github.io/mark/reference/labels.md)
  gains an additional argument `.missing` to set how to control for
  missing labels: you can now use a `warning` for a missing label
  (instead of an error) or silently ignore any missing labels
- [`sort_names()`](https://jmbarbone.github.io/mark/reference/sort_names.md)
  gains a new argument `numeric` to try to sort names of `x` by their
  numeric value [\#26](https://github.com/jmbarbone/mark/issues/26)
- adds
  [`struct()`](https://jmbarbone.github.io/fuj/reference/struct.html), a
  simplified version of
  [`struct()`](https://jmbarbone.github.io/fuj/reference/struct.html)
- adds more
  [`fact()`](https://jmbarbone.github.io/mark/reference/fact.md) methods
  - [`fact.integer()`](https://jmbarbone.github.io/mark/reference/fact.md)
    [\#30](https://github.com/jmbarbone/mark/issues/30)
  - [`fact.haven_labelled()`](https://jmbarbone.github.io/mark/reference/fact.md)
    [\#31](https://github.com/jmbarbone/mark/issues/31)
- [`todos()`](https://jmbarbone.github.io/mark/reference/todos.md) and
  [`fixmes()`](https://jmbarbone.github.io/mark/reference/todos.md) gain
  an additional argument `path` to specify a directory or file to search
  within [\#25](https://github.com/jmbarbone/mark/issues/25)
- [`print.pseudo_id()`](https://jmbarbone.github.io/mark/reference/print.pseudo_id.md)
  added for a cleaner print
- [`between_more()`](https://jmbarbone.github.io/mark/reference/within.md)
  accepts vectors for `left` and `right` params

### Non visible changes

- code coverage added
- additional tests added

## mark 0.1.4

CRAN release: 2021-07-16

- no visible user changes
- removes temporarily created files
  [\#22](https://github.com/jmbarbone/mark/issues/22)

## mark 0.1.3

CRAN release: 2021-06-16

New name! The previous name `jordan` was conflicting with recent package
on CRAN.

### Changes

- corrects use of `...` in
  [`todos()`](https://jmbarbone.github.io/mark/reference/todos.md)
  [\#8](https://github.com/jmbarbone/mark/issues/8)
  - [`grep()`](https://rdrr.io/r/base/grep.html) also now evaluated with
    *cleaned* todo text
  - searches for
    [`todos()`](https://jmbarbone.github.io/mark/reference/todos.md) in
    Rmd files, too
  - correctly removes additional `#` and spaces in lines (e.g.,
    `# # TODO text` -\> `text`)
- updates for
  [`counts()`](https://jmbarbone.github.io/mark/reference/counts.md)
  - corrects `NA` counting in
    [`counts()`](https://jmbarbone.github.io/mark/reference/counts.md);
    `NA` counts are now appended at the end whether or not sort is
    called
  - other optimization for
    [`counts()`](https://jmbarbone.github.io/mark/reference/counts.md)
  - core functions previously on
    [`base::rle()`](https://rdrr.io/r/base/rle.html) now use a
    combination of
    [`pseudo_id()`](https://jmbarbone.github.io/mark/reference/pseudo_id.md)
    and [`base::tabulate()`](https://rdrr.io/r/base/tabulate.html)
  - corrects counts for factor data when higher levels are not present
    [\#16](https://github.com/jmbarbone/mark/issues/17)
- update to
  [`multi_grepl()`](https://jmbarbone.github.io/mark/reference/multi_grepl.md)
  internal functions to prevent conflicts with `R 4.1.0`
- corrects error message in
  [`vector2df()`](https://jmbarbone.github.io/mark/reference/vector2df.md)
  when passed a list

### New features

- adds/exports
  [`fact()`](https://jmbarbone.github.io/mark/reference/fact.md) and
  [`pseudo_id()`](https://jmbarbone.github.io/mark/reference/pseudo_id.md)
- adds [`fixmes()`](https://jmbarbone.github.io/mark/reference/todos.md)
  [\#13](https://github.com/jmbarbone/mark/issues/13)
- adds
  [`names_switch()`](https://jmbarbone.github.io/mark/reference/set_names0.md)
  to switch names and values
- [`vector2df()`](https://jmbarbone.github.io/mark/reference/vector2df.md)
  can now output a 1 column data.frame if `name = NULL`
- adds an `invert` parameter to
  [`complete_cases()`](https://jmbarbone.github.io/mark/reference/complete_cases.md)
  to filter for incomplete cases
- adds
  [`are_identical()`](https://jmbarbone.github.io/mark/reference/are_identical.md)
  for comparing 2 or more vectors as
  [`identical()`](https://rdrr.io/r/base/identical.html), element-wise
- adds
  [`add_file_timestamp()`](https://jmbarbone.github.io/mark/reference/add_file_timestamp.md)
- [`diff_time()`](https://jmbarbone.github.io/mark/reference/diff_time.md)
  and related functions will try to convert `y` to a `Date` object if
  `x` is passed as date (e.g.,
  `diff_time_days(Sys.Date(), "2021-06-03")` will not show decimals)
