## Test environments
* local R installation, R 4.1.0
* windows-latest, release 4.0, 3.6
* macOS-latest, release, 4.0, 3.6
* ubuntu 20.04, release, 4.0, 3.6

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.

## Resubmission
This is a resubmission.  In this version I have:

* revises the package description in DESCRIPTION
* adds build date/time
* added \value{} tags for functions
  * base_alpha.Rd
  * chr_split.Rd
  * ept.Rd
  * eval_named_chunk.Rd
  * file_utils.Rd
  * flip.Rd
  * get_dir_max_number.Rd
  * get_dir_recent_date.Rd
  * get_version.Rd
  * grapes-colons-grapes.Rd
  * import.Rd
  * list_environments.Rd
  * logic_ext.Rd
  * match_arg.Rd
  * match_ext.Rd
  * match_param.Rd
  * median2.Rd
  * muffle.Rd
  * na_assignments.Rd
  * not_available.Rd
  * note.Rd
  * quiet_stop.Rd
  * range2.Rd
  * recode_by.Rd
  * reindex.Rd
  * remove_na.Rd
  * remove_null.Rd
  * require_namespace.Rd
  * round_by.Rd
  * script.Rd
  * save_source.Rd
  * set_names0.Rd
  * simpleTimeReport.Rd
  * sort_by.Rd
  * sort_names.Rd
  * source_files.Rd
  * source_to_env.Rd
  * sourcing.Rd
  * str_slice.Rd
  * switch-ext.Rd
  * that.Rd
  * to_boolean.Rd
  * use_author.Rd
  * utils-paste.Rd
  * with_par.Rd
  * within_call.Rd
* removed \dontrun{}
  * checkOptions.Rd
  * clipboard.Rd (runs conditionally on windows)
  * fizzbuzz.Rd (uses \donttest{})
  * format_to_regex.Rd
  * import.Rd
  * mapply0.Rd (uses try())
  * norm_path.Rd
  * pseudo_id.Rd
  * range2.Rd
  * read_bib.Rd (conditionally runs with available packages; uses \donttest{})
  * recode_by.Rd
  * require_namespace.Rd (uses try())
  * split_expression.Rd
  * switch-ext.Rd (uses try())
  * with_par.Rd
* removed commented out code
  * lables.Rd
* references
  * there are no references to methods
