# jordan 0.2.0

* Added a `NEWS.md` file to track changes to the package
* Files renamed and reorganized
* Various tests included
* Added aliases to muffle messages and warnings (`muffle()` and `wuffle()`)
* Added `limit()`
* `match_param()` now returns correct errors messages for calls
* `write_clipboard()` formats vectors as characters
* `read_clipboard()` tries to correctly format vectors and data.frames

## Moved to `jordanExtra`

Some miscellaneous, less controlled functions have been moved to [jordanExtra](github.com/jmbarbone/jordanExtra).

* Functions for Rust `set_rust_engine()`, `engine_rust()`
* Functions for [openxlsx](): `add_data_sheet()`, `add_image_sheet()`
* Functions for [pROC](): `pROC_optimal_threshold()`, `pROC_quick_plot()`
* Effect sizes: `cohen2odds()`, `cohend2r()`, `odds_ratio()`, `odds2d()`, `odds2r()`, `r2cohend()`
* Statistical functions: `fishers_method()`, `iqrs()`, `p_round()`, `p_value_sig()`, `percentile_rank()`, `proportion()`, `sd_pooled()`, `sterr()`, `tukey_coef()`, `z_score()`
* Others: `add_euclidean()`, `add_malahanobis()`, `%=+`, `filter_combine()`, `reverse_log_trans()`, 

# jordan 0.1.0

* Initial commits of functions
* Quite a mess with no versions
