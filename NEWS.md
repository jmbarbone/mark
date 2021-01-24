# development

* adds `fizzbizz()`
* adds `quick_df()`
* adds `complete_cases()`
* adds listing shorhands:
    * `ls_object()` to list all `is.object()`s 
    * `ls_dataframe()` to list all `is.data.frame()`s 
    * `ls_function()` to list all `is.function()`s 
* Updates to prevent conflicts with popular packages
  * `%||%` is no longer exported
  * `collapse()` is now `collapse0()` -- although `glue::collapse()` is meant to be deprecated
  * `set_names()` is now `set_names0()`
* `do_paste_combine()` (used inside `paste_combine()`) simplified to remove use of `outer()`


# jordan 0.1.0s

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
