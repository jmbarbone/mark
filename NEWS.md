# mark (development version)

* `make_sf()` now uses `substitute()` to provide more information inside the function body when viewing [#129](https://github.com/jmbarbone/mark/issues/129)
* `file_copy_md5()` now produces more messages [#239](https://github.com/jmbarbone/mark/issues/239)
* `file_copy_md5()` saves md5 sum checks as an attribute

# mark 0.8.2

* `read_clipboard()` and `write_clipboard()` now use `{clipr}` to work on non-Windows platforms [#125](https://github.com/jmbarbone/mark/issues/125)
* `read_clipboard()` now works with more methods for reading `data.frame`s
* `read_clipboard()` now defaults to a `tibble` return when `{tibble}` is available
* timezone testing updated [#247](https://github.com/jmbarbone/mark/issues/247)
* actions updated

# mark 0.8.1

* `write_file_md5()` now supports `"feather"` and `"parquet"` methods as wrappers for [`{arrow}`](https://arrow.apache.org/docs/r/) [#234](https://github.com/jmbarbone/mark/issues/234)
* `md5()` added to provide MD5 check sums for objects [#233](https://github.com/jmbarbone/mark/issues/233)
* `unique_rows()` added to subset on (non-)duplicated rows in a `data.frame` [#87](https://github.com/jmbarbone/mark/issues/87)
* `within()` added as an alternative to `between_more()` [#120](https://github.com/jmbarbone/mark/issues/120)
* test updated for upcoming R release [#240](https://github.com/jmbarbone/mark/issues/240)

# mark 0.8.0

## breaking changes

* `echo()` is now removed; use `echo::echo()` instead [#214](https://github.com/jmbarbone/mark/issues/214)
* includes `tryn()` for running an expression a maximum number of times before failure [#80](https://github.com/jmbarbone/mark/issues/80)

## fixes

* `unlist0()` no longer fails when input list is not named [#220](https://github.com/jmbarbone/mark/issues/220)

## improvements

* `match_param()` has been improved
  * can now return multiple matches [#191](https://github.com/jmbarbone/mark/issues/191), and can return partial matches
  * error message readability improved for `matchParamMatchError`[#194](https://github.com/jmbarbone/mark/issues/194)
  * `choices` can now be a list of `formula` elements, preserving the return value

## new features

* `file_copy_md5()` added as a wrapper for `fs::file_copy()` but provides MD5 checks through `tools::md5sum()` to avoid overwriting files that had no content changes [#207](https://github.com/jmbarbone/mark/issues/207)
* `write_file_md5()` added as a general writing function and utilizes `file_copy_md5()` for MD5 checks (including some compression options) [#207](https://github.com/jmbarbone/mark/issues/207), [#224](https://github.com/jmbarbone/mark/issues/224)

# mark 0.7.0

* `merge_list()` added for combining lists [#200](https://github.com/jmbarbone/mark/issues/200)
* `glob()` added for basic wildcard globbing on character vectors [#167](https://github.com/jmbarbone/mark/issues/167)
* adds greater use of `{fs}` over base file functions [#160](https://github.com/jmbarbone/mark/issues/160)
* improvements in `todos()` and `fixmes()`
  * File extension can now be set [#170](https://github.com/jmbarbone/mark/issues/170), which by default includes `qmd` ([#163](https://github.com/jmbarbone/mark/issues/163)) and `py` files
  * new parameter `ignore` to ignore any files
  * file paths and line numbers can now be _clicked_ within RStudio [#171](https://github.com/jmbarbone/mark/issues/171)
* adds more use of `rlang::list2()` for internally [#199](https://github.com/jmbarbone/mark/issues/199)
* GitHub action included to check version updates with pull requests [#211](https://github.com/jmbarbone/mark/issues/211)
* `%::%` and `%:::%` now exported from `{fuj}`

# mark 0.6.1

* updates Timezone references for upcoming R release [#203](https://github.com/jmbarbone/mark/issues/203)

# mark 0.6.0

## New features

* new functions for detecting _blank_ values in a vector or `data.frame`.  _Blank_ values are those which do not contain any text (controls for `NA`) or are entirely white space.
  * `is_blank()` for detecting _blank_ values in a vector
  * `is_blank_cols()` for detecting _blank_ columns
  * `select_blank_cols()` for selecting _blank_ columns
  * `remove_blank_cols()` for removing _blank_ columns
* `match_param()` now accepts a named listed for alias matching [#104](https://github.com/jmbarbone/mark/issues/104)
* `echo()` evaluates expressions and logs outputs [#164](https://github.com/jmbarbone/mark/issues/164)
* `{fuj}` is now imported
  * multiple functions now re-exported from `{fuj}` (see `?mark::reexports`)
  * `set_names0()` is deprecated in favor of `set_names()`
  * error messages are created with `fuj::new_condition()`;
  * test for errors and warnings enhanced with class checks

## Fixes and updates

* `date_from_partial()` works again [#155](https://github.com/jmbarbone/mark/issues/155) after fixing an issue with an internal utility `is_valid_date_string()` that wasn't recognizing `%Y-%m-%d` (and potentially others)
* `lintr` GitHub action updated [#173](https://github.com/jmbarbone/mark/issues/173); this includes plenty of internal improvements and code cleanup
* package description in help files corrected [#165](https://github.com/jmbarbone/mark/issues/165)
* GitHub pages updated with latest `{pkgdown}` action [#175](https://github.com/jmbarbone/mark/issues/175)
* Update to GitHub R-CMD-check action [#178](https://github.com/jmbarbone/mark/issues/178)
* `switch_in_case()` handles `NA`s better [#183](https://github.com/jmbarbone/mark/issues/183)
* internal `switch` tests updated for `{waldo}` development [#182](https://github.com/jmbarbone/mark/pulls/182) thanks, `@hadley`
* methods for `write_clipboard()` are now displayed in documentation [#186](https://github.com/jmbarbone/mark/issues/182)

# mark 0.5.3

* CRAN fix for new release [#151](https://github.com/jmbarbone/mark/issues/151)

# mark 0.5.2

## New features

* `normalize()` added to normalize values in `vectors`, `matrices`, and `data.frame`s by specified ranges and boundaries [#143](https://github.com/jmbarbone/mark/issues/143)
* `get_labels()` and other label related functions now get exact matches for `"label"` attributes [#141](https://github.com/jmbarbone/mark/issues/141)
* `recode_only()`, and `recode_by()` now accept a named `list()` for `by` [#96](https://github.com/jmbarbone/mark/issues/96)]
* `switch_in_case()` now handles functions in the right hand statements
* `update_version()` now correctly checks result of embedded `utils::menu()` call for updating the version [#123](https://github.com/jmbarbone/mark/issues/121)
* `require_namespace()` now accepts multiple namespaces [#121](https://github.com/jmbarbone/mark/issues/121)
* `unique.fact()` S3 method [#86](https://github.com/jmbarbone/mark/issues/86)
* `recode_only()` and `recode_by()` can accept a single value for `val` [#73](https://github.com/jmbarbone/mark/issues/73)
* `fact_reverse()` for reversing `fact` levels [#78](https://github.com/jmbarbone/mark/issues/78)
* `as.Date.fact()` added [#108](https://github.com/jmbarbone/mark/issues/108)
* `as.character.fact()` added
* `[.fact` added
* `read_bib()` better handles fields where `=` is present in the text [#117](https://github.com/jmbarbone/mark/issues/117)

## Fixes

* `fact.haven_labelled()` works properly and retains the `label` attribute [#136](https://github.com/jmbarbone/mark/issues/136)
* `drop_levels()` is exported [#105](https://github.com/jmbarbone/mark/issues/105)
* `recode_by()` and `recode_only()` handle factors better [#81](https://github.com/jmbarbone/mark/issues/81)
* Functions that made use of `shell.exec()` now try to determine the appropriate method of opening a file base on OS. [#126](https://github.com/jmbarbone/mark/issues/126)
* Internal functions for potentially coercing factor levels into dates no longer try to check for `"%Z"` in the date format [#147](https://github.com/jmbarbone/mark/issues/147)

## Breaking changes

* `reverse()` has been removed (use `flip()` instead)
* `assign_label()` has been removed (use `assign_labels()` instead)
* `percentile_rank(times)` is deprecated in favor of `percent_rank(weights)`

## Non visible changes

* `print.fact()` rewritten as a slightly modified `print.factor()` [#109](https://github.com/jmbarbone/mark/issues/109)
* `percentile_rank()` improvements [#131](https://github.com/jmbarbone/mark/issues/131)

# mark 0.5.1

* Fix for CRAN check [#128](https://github.com/jmbarbone/mark/issues/128)

# mark 0.5.0

The package website https://jmbarbone.github.io/mark/ is now available!
More references and vignettes will make their way here in future releases.

## Fixes

* `detail(NA)` (or when `x` has only `NA` values) no longer throws a warning and returns `NA` for `min_c`, `max_c` [#59](https://github.com/jmbarbone/mark/issues/59)
* `print.noted()` now passes `...` to next methods [#67](https://github.com/jmbarbone/mark/issues/67)
* corrects deprecation warning in `assign_label()` [#74](https://github.com/jmbarbone/mark/issues/74)
  * `assign_label()` will be removed in `0.4.2`
* `set_not_available()` now seems to work correctly -- it probably hasn't actually be working most of the time
* `percentile_rank()` is now more correct when `x` is a decimal by checking for unique values first [#92](https://github.com/jmbarbone/mark/issues/92)
* `counts.data.frame()` now handle factor columns better

## New features

* Functions in `?handlers`, all allow for additional params passed through `...` [#34](https://github.com/jmbarbone/mark/issues/34)
* adds `row_bind()` to bind a list of `data.frames()` [#46](https://github.com/jmbarbone/mark/issues/46)
 * adds `drop_levels()` with `factor` and `data.frame` methods; functions similarly to `base::droplevels()` but is a little faster [#54](https://github.com/jmbarbone/mark/issues/54)
* `todos()` and `fixmes()` gain a new param `force`
  * When `TRUE`, forces searches for `.R` files when the given directory does not contain an `.Rproj` file
  * When `FALSE`, prevents long start ups when these functions are called in a `.Rprofile` file and R is not launches in a project directory
  * This be toggled with a new options `mark.todos.force`
* adds `set_note()`, a wrapper for `note<-()` [#77](https://github.com/jmbarbone/mark/pull/77)
* adds `fact2char()` to compliment `char2fact()` [#75](https://github.com/jmbarbone/mark/pull/75)
* `print.pseudo_id()` now truncates long uniques to a single line [#70](https://github.com/jmbarbone/mark/pull/70)
* `match_param(NULL, null = TRUE)` allows `param` to safely return `NULL` [#89](https://github.com/jmbarbone/mark/issues/89)
* `fact_na()` is added to use `fact` vectors with `NA` levels that work with `is.na()` [#69](https://github.com/jmbarbone/mark/issues/69) and other `NA` handling improvements
* adds a new `print._mark_bib_df()` method to supporting printing lists
* adds new methods for `facts`: `as.integer.fact()`, `as.double.fact()`, `remove_na.fact()`

## Breaking changes

* `fact.numeric()` now treats `NaN` the same as `NA`, no extra level/unique value is retained
* `read_clipboard()` now returns `NA` when the clipboard is empty, rather than `""` (improvements with internal type conversions)
* improvements to `NA` handling as well

## Other, non-visible

* github actions updated
* internal type conversion now heavily relies on `utils::type.convert()` with some additional functionality for logical (e.g., character string using `"true"` and `"false"`) and for guessing dates in a `YYYY-MM-DD` format
* general clean up and formatting

# mark 0.4.1

* `details(factor)` no longer adds `fact` class to `factors` [#50](https://github.com/jmbarbone/mark/issues/50)
* `details()` gains new argument `factor_n` to control threshold for making character vectors into factors
* `detail.data.frame()` now works with single column data.frames [#48](https://github.com/jmbarbone/mark/issues/48)
* `paste_combine()` no longer duplicated the second vector of `...` when `length(...) > 2` [#52](https://github.com/jmbarbone/mark/issues/52)

# mark 0.4.0

## New features

* adds `percentile_rank()` to calculate percentile ranks with a vector
* adds `insert()` to insert multiple values into a vector
* `pseudo_id()` gains argument `na_last` to change positioning of `NA` values
* `is_true()` and `is_false()` are now exported as generics with methods for `default` and `logical`
* adds `omit_na()` for tracking positions of `NA` and non-`NA` values
* `quick_df(NULL)`  now returns an empty `data.frame`
* `quick_dfl()` exported as a wrapper for `quick_df(list(...))`

## Fixes

* `squash_vec()` now works correctly when values are not ordered [#43](https://github.com/jmbarbone/mark/issues/43)
* `as_ordered()` no longer duplicates `ordered` class [#44](https://github.com/jmbarbone/mark/issues/44)
* `counts.data.frame()` and `props.data.frame()` correctly make column names unique
* internal `try_numeric()` correctly handles `NA`s
* `flip.matrix(, keep_rownames = FALSE)` now works correctly
* `any_match()` now works as expected
* `lines_of_r_code()` now works correctly reading a single file
* `import(, overwrite = TRUE)` now works
* `ls_function()`, `ls_object()`, `ls_dataframe()`, and `ls_all()` have improvements for environmental searching
* `assign_labels.data.frame(.missing = "warning")` correctly removes missing columns
* `remove_na.factor()` no long drops additional classes other than `ordered` and `factor`

## Others

* documentation of `struct()` overwriting attributes improved and examples
* adds more unit tests

# mark 0.3.0

## Fixes

* `fact.haven_labelled()` now returns an object with class `fact` [#39](https://github.com/jmbarbone/mark/issues/39); performance enhancements
* `set_names0(NULL)` no longer causes an error and returns `NULL` [#40](https://github.com/jmbarbone/mark/issues/40)
* `diff_time()` correctly handles time zones when `x` is `Date` and `y` is `POSIXt` [#41](https://github.com/jmbarbone/mark/issues/41)

## Changes

* updates file path finding functions (e.g., `list_files()`) to try to not search every file depending on desired searches (e.g., by full file paths or just base names)
* `as_ordered()` handles `factors` better; S3 methods removed: `as_ordered.ordered()`, `as_ordered.factor()`
* `remove_na()` has better performance when `x` has no `NA` values
* `counts.data.frame()` and `props.data.frame()` retain attributes of selected columns
* `todos()` and `fixmes()` will not search for `.R` or `.Rmd` files if the `path` is not changed from `""` and no `.Rproj` is found within the directory

## New features

* adds `unlist0()` to retain original names of lists
* adds `%names%` for a fun way to set names
* adds `file_open()` as alias for `open_file()`
* adds `detail()` to return a `data.frame` of details for a vector of columns of a `data.frame`
* adds `squash_vec()` to combined the names of a vector with repeated values
* adds `make_sf()` as a simple wrapper for package specific `system.file()`
* `add_file_timestampe()` gains a new parameter `sep` to separate the file name (sans ext) and the time stamp
* `assign_labels.data.frame()` gains new argument `.ls` to explicitly set a `list` (or `data.frame`) of columns
* `props()` and family gain argument `na.rm` to not count `NA` values for proportions

# mark 0.2.0

## Changes

* `package_available()` now visibly returns `TRUE`/`FALSE`
* `remove_na()` now has methods for `list`s and `factor`s
* `environments()` now has it's own `print.mark_environments()` method rather than calling `cat()` within the function itself
* `array_extract()`'s first argument is changed from `arr` to `.arr`
* `diff_time()` now defaults to using UTC (Related to [#32](https://github.com/jmbarbone/mark/issues/32))
* `print.note()` method has been updated (Related to: [#33](https://github.com/jmbarbone/mark/issues/33)):
  * to print `x` _normally_, without the `note class` when just the note has to be seen
  * an internal function now handles the note formatted for class `noted`
* changes to `fact()`
  * `fact()` now returns a vector with a `fact` element
  * `fact.character()` correctly labels `NA`s [#24](https://github.com/jmbarbone/mark/issues/24)
  * `fact.factor()` not longer simply returns `x` but rather updates the levels and integer values to confirm with other `fact()` methods.  `fact.factor()` will retain all levels of the original value, reorder the levels, and append `NA` if necessary
  * `fact.fact()` added to return a correctly formatted `fact()`
  * `fact.logical()` now orders levels as `TRUE` then `FALSE`, and `NA` if present
  * `fact.Date()` and `fact.POSIXt()` added, which simply call `fact.numeric()`
  * `print.fact()` method added to print a `fact` vector as a `factor`
  * `as_ordered.factor()` and `as_ordered.ordered()` now call `fact()` to check levels

## Fixes

* functions that check if an argument is a vector no long use `is.vector()` directly; arguments passed with attributes that when removed fulfill `is.vector()` are accepted
* `todos()` and `fixmes()` now correctly show tags for `.Rmd` files
* correction to error message in `limit()`
* adds missing `sort` argument to `props()`
* `pseudo_id.factor()` no longer returns `NA_integer` when a value is `NA` or a level is `NA` and correctly resets the order of the levels from the factor to their order of appearance
* `flip.data.frame()` no longer coerces single column data.frames [#36](https://github.com/jmbarbone/mark/issues/36)

## New features

* `fact.pseudo_id()` and `pseudo_id.pseudo_id()` methods added
* adds `as_ordered()` to quickly create `ordered` factors using `fact()`
* adds `char2fact()` to convert `character` vectors (or columns in a `data.frame`) to `factors` based on the number unique values
* adds `tableNA()` to make a table from `NA` values
* `round_by()` gains an additional argument `include0` which if `FALSE` will replace `0` values with `by`
* `assign_labels.data.frame()` gains an additional argument `.missing` to set how to control for missing labels: you can now use a `warning` for a missing label (instead of an error) or silently ignore any missing labels
* `sort_names()` gains a new argument `numeric` to try to sort names of `x` by their numeric value [#26](https://github.com/jmbarbone/mark/issues/26)
* adds `struct()`, a simplified version of `struct()`
* adds more `fact()` methods
  * `fact.integer()` [#30](https://github.com/jmbarbone/mark/issues/30)
  * `fact.haven_labelled()` [#31](https://github.com/jmbarbone/mark/issues/31)
* `todos()` and `fixmes()` gain an additional argument `path` to specify a directory or file to search within [#25](https://github.com/jmbarbone/mark/issues/25)
* `print.pseudo_id()` added for a cleaner print
* `between_more()` accepts vectors for `left` and `right` params

## Non visible changes

* code coverage added
* additional tests added

# mark 0.1.4

* no visible user changes
* removes temporarily created files [#22](https://github.com/jmbarbone/mark/issues/22)

# mark 0.1.3

New name!
The previous name `jordan` was conflicting with recent package on CRAN.

## Changes

* corrects use of  `...` in `todos()` [#8](https://github.com/jmbarbone/mark/issues/8)
  * `grep()` also now evaluated with _cleaned_ todo text
  * searches for `todos()` in Rmd files, too
  * correctly removes additional `#` and spaces in lines (e.g., `#  # TODO text` -> `text`)
* updates for `counts()`
  * corrects `NA` counting in `counts()`; `NA` counts are now appended at the end whether or not sort is called
  * other optimization for `counts()`
  * core functions previously on `base::rle()` now use a combination of `pseudo_id()` and `base::tabulate()`
  * corrects counts for factor data when higher levels are not present [#16](https://github.com/jmbarbone/mark/issues/17)
* update to `multi_grepl()` internal functions to prevent conflicts with `R 4.1.0`
* corrects error message in `vector2df()` when passed a list

## New features

* adds/exports `fact()` and `pseudo_id()`
* adds `fixmes()` [#13](https://github.com/jmbarbone/mark/issues/13)
* adds `names_switch()` to switch names and values
* `vector2df()` can now output a 1 column data.frame if `name = NULL`
* adds an `invert` parameter to `complete_cases()` to filter for incomplete cases
* adds `are_identical()` for comparing 2 or more vectors as `identical()`, element-wise
* adds `add_file_timestamp()`
* `diff_time()` and related functions will try to convert `y` to a `Date` object if `x` is passed as date (e.g., `diff_time_days(Sys.Date(), "2021-06-03")` will not show decimals)

# jordan 0.1.2

## Changes

* removes checks for `stringsAsFactors` option in `quick_df()`
* improves functionality of `note`
  * removes `jordan.note.fun` option for printing -- this was too complicated and doesn't seem to work too well without too many adjustments
  * `note<-` now appends the class `noted` to the object so that a `print.noted` method is dispatched so the note will be printed when called
  * `print.note` note defaults to a colorful _message_ called with `cat()`
  * Startup related functions moved to [`jordanExtra`](https://github.com/jmbarbone/markExtra); these were a bit _wild_, dynamic, and not well tested.  The **.Rprofile** template also exists in the separate package.

## Improvements

* `match_param()` now reports the value passed to `param` on failure
* improvements to handlers
  * adds `has_message()` and `get_message()`
  * internal rework of `catch()` for catching errors, messages, and warnings
  * `has_*()` now returns the result in the `result` attribute
  * `print.has_catch()` will hide attributes in print
* improvements/updates to `todos()`
  * allows text filtering by passing arguments to `grep()`
  * adds new print method for `todos_df` for viewing tasks
  * the result of `todos()` still has class `data.frame` but will now also have class `todos_df`
  * the `print.todos_df()` method should be make more sense for task management
* `str_slice_by_word()` no longer has a leading `" "` for each element after the first (this was not the intention of the split)
* `is_file()` and `is_dir()` now returns and error when passed `NULL` or a vector of length 0
* `switch_params()` now accepts a vector for `x`
  * `...` examples updated
* adds param to change default column name from `counts.data.frame()` and `props.data.frame()`
* `print()` method called from `todos()` has a new format to group together multiple items found in a single file

## New features

* adds `print_c()` to print a vector into a `c()` "string"
* adds `diff_time` functions
  * functions include `diff_time()` along with shortcuts for specific methods: `diff_time_secs()`, `diff_time_days()`, `diff_time_years()`, and others (see `?jordan::diff_time`)
  * these are much like the `base::difftime()` but...
    * have more methods/units for computing differences, which may need to be the case for when a unit of time has to be _standardized_ (e.g., 1 year needs to be 365 days, or 1 month needs to be 30 days)
    * are class `diff_time` and `numeric`
    * have a slightly different print method (will note appropriately how units of time are measures)
    * can account for timezone differences (must be set with a the `tza` and `tzb` parameters)
* adds `sort_by()`
* adds `NA` assignments:
    * `NA_at()` for position assignments
    * `NA_if()` for logical/conditional assignments
    * `NA_in()` for inclusive matching assignments
    * `NA_out()` for exclusive matching assignments
* adds functions for sourcing scripts into environments for later use (`rscript()`, `save_source()`, and `source_to_env()`)
* adds `switch_case()` to return a values based on a left hand statement
  returning `TRUE` and `switch_in_case()` for evaluating `x` `%in%` left hand side
  * these functions are much like `dplyr::case_when()` but for specific cases
  * adds function `checkOptions()`
* adds `recode_by()` and `recode_only()` for a simple implementations of recoding elements in a vector

# jordan 0.1.1

## New features

* adds `fizzbuzz()`
* adds data.frame functions
  * adds `quick_df()` to turn a list into a data.frame (used internally, too)
  * adds `complete_cases()` to select rows without `NA` values
  * removes `show_NA` parameter from `vector2df()` and `list2df()`
    * for vectors this will now produce an NA value for the first column
    * for lists `make.unique()` is utilized for empty name named to retain the position of the list element
* adds listing wrapper:
  * `ls_object()` to list all `is.object()`s
  * `ls_dataframe()` to list all `is.data.frame()`s
  * `ls_function()` to list all `is.function()`s
* adds `counts()` and `props()` for counting unique elements in vectors and data.frames

## Changes

Some exported functions and names have been changed to prevent conflicts with other popular packages

* `%||%` is no longer exported; it is exported in `rlang` (and reexported in `purrr`) and is a relatively simply function anyway
* `collapse()` is now `collapse0()` to avoid conflicts with `glue`; although `glue::collapse()` is meant to be deprecated, `collapse0()` is mostly a wrapper for `paste0()`, so this may be a better name
* `set_names()` is now `set_names0()` to avoid conflicts with `rlang` (reexported from `purrr`) and `magrittr`

## Fixes/updates

* `do_paste_combine()` (used inside `paste_combine()`) simplified to remove use of `outer()`
* improves version bumping/updating
  * added `get_version()` to retrieve the current package version (assuming you're in the directory)
  * `utils::menu()` is called to confirm that version should be updated
  * can update by either adding a number to the version (`bump_version()` or by date `bump_date_version()`)
* implements an improved non-exported `string_extract()` function inside `str_extract_date()` and `str_extract_datettime()`

# jordan 0.1.0

Major cleanup for documenting, reviewing, removing, relocating, and testing functions.

* Added a `NEWS.md` file to track changes to the package
* Files renamed and reorganized
* Various tests included
* Added aliases to muffle messages and warnings (`muffle()` and `wuffle()`)
* Added `limit()`
* `match_param()` now returns correct errors messages for calls
* `write_clipboard()` formats vectors as characters
* `read_clipboard()` tries to correctly format vectors and data.frames
* removed `str_close_enough()` because this didn't make much sense anyway

## New functions

* `%colons%`: A substitute for `::` and `:::`
* `%||%`: Virtually identical to [rlang's version](https://github.com/r-lib/rlang/blob/0bce15a1e0ade47347c776cdbd823c4dbf2a527b/R/operators.R)
* `.CharacterIndex()`: Quick visual of indexes in character strings
* `bump_date_version`: Updated DESCRIPTION versions that are saved as dates
* `chr_split()`: Essentially an alias for `strsplit(., "")[[1]]`
* Error and warning handlers: `get_error()`, `get_warning()`, `has_error()`, `has_warning()`, `muffle()` and `wuffle()` (aliases for `suppressMessages()` and `suppressWarnings()`)
* File/directory utilities: `is_dir()`, `is_file()`, `list_dirs()`
* `is_na_cols()`: previously not exported, inside `select_na_cols()` and `remove_na_cols()`
* `limit()`: Limits a numeric vector by an upper and lower bound
* `match_param()`: Alternative to `match.arg()` without partial matching and more detailed error message
* Functions for calls: `outer_call()`, `outer_fun()`, `within_call()`, `within_fun()`
* `quiet_stop()`: calls `stop()` without throwing an error
* Functions for names: `set_names()`, `remove_names()`
* `require_namespace()`: Mostly a wrapper for `require()` with a more detailed error message
* `vap_*()`: Wrappers for `vapply()`, must like [purrr's map_*](https://github.com/tidyverse/purrr/blob/761a2224c437067c5d07beeeba06cde65a3e08a6/R/map.R)

## Moved to `jordanExtra`

Some miscellaneous, less controlled functions have been moved to [jordanExtra](https://github.com/jmbarbone/markExtra).

* Functions for Rust `set_rust_engine()`, `engine_rust()`
* Functions for [openxlsx](https://github.com/ycphs/openxlsx): `add_data_sheet()`, `add_image_sheet()`
* Functions for [pROC](https://github.com/xrobin/pROC): `pROC_optimal_threshold()`, `pROC_quick_plot()`
* Effect sizes: `cohen2odds()`, `cohend2r()`, `odds_ratio()`, `odds2d()`, `odds2r()`, `r2cohend()`
* Statistical functions: `fishers_method()`, `iqrs()`, `p_round()`, `p_value_sig()`, `percentile_rank()`, `proportion()`, `sd_pooled()`, `sterr()`, `tukey_coef()`, `z_score()`
* Others: `add_euclidean()`, `add_malahanobis()`, `%=+`, `filter_combine()`, `reverse_log_trans()`,

# 0.0.9000

* Initial commits of functions
* Quite a mess with no versions
