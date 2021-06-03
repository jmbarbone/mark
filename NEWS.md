# development

* corrects use of  `...` in `todos()` [#8](https://github.com/jmbarbone/jordan/issues/8)
  * `grep()` also now evaluated with _cleaned_ todo text
  * searches for `todos()` in Rmd files, too
  * correctly removes additional `#` and spaces in lines (e.g., `#  # TODO text` -> `text`)
* updates for `counts()`
  * corrects `NA` counting in `counts()`; `NA` counts are now appended at the end whether or not sort is called
  * other optimization for `counts()`
  * core functions previously on `base::rle()` now use a combination of `pseudo_id()` and `base::tabulate()`
  * corrects counts for factor data when higher levels are not present [#16](https://github.com/jmbarbone/jordan/issues/17)
* adds/exports `fact()` and `pseudo_id()`
* adds `fixmes()` [#13](https://github.com/jmbarbone/jordan/issues/13)
* adds `names_switch()` to switch names and values
* adds an `invert` parameter to `complete_cases()` to filter for incomplete cases
* `vector2df()`
  * can now output a 1 column data.frame if `name = NULL`
  * corrects error message when passed a list
* adds `are_identical()` for comparing 2 or more vectors as `identical()`, element-wise
* adds `add_file_timestamp()`

# jordan 0.1.2

## Changes

* removes checks for `stringsAsFactors` option in `quick_df()`
* improves functionality of `note`
  * removes `jordan.note.fun` option for printing -- this was too complicated and doesn't seem to work too well without too many adjustments
  * `note<-` now appends the class `noted` to the object so that a `print.noted` method is dispatched so the note will be printed when called
  * `print.note` note defaults to a colorful _message_ called with `cat()`
  * Startup related functions moved to [`jordanExtra`](github.com/jmbarbone/jordanExtra); these were a bit _wild_, dynamic, and not well tested.  The **.Rprofile** template also exists in the separate package.

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

Some miscellaneous, less controlled functions have been moved to [jordanExtra](https://github.com/jmbarbone/jordanExtra).

* Functions for Rust `set_rust_engine()`, `engine_rust()`
* Functions for [openxlsx](https://github.com/ycphs/openxlsx): `add_data_sheet()`, `add_image_sheet()`
* Functions for [pROC](https://github.com/xrobin/pROC): `pROC_optimal_threshold()`, `pROC_quick_plot()`
* Effect sizes: `cohen2odds()`, `cohend2r()`, `odds_ratio()`, `odds2d()`, `odds2r()`, `r2cohend()`
* Statistical functions: `fishers_method()`, `iqrs()`, `p_round()`, `p_value_sig()`, `percentile_rank()`, `proportion()`, `sd_pooled()`, `sterr()`, `tukey_coef()`, `z_score()`
* Others: `add_euclidean()`, `add_malahanobis()`, `%=+`, `filter_combine()`, `reverse_log_trans()`,

# Prior development

* Initial commits of functions
* Quite a mess with no versions
