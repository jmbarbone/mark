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

* reviewed \value{} tags; removed Rd files for non-exported functions
  * counts_n.Rd
  * diff_time.Rd
  * do_read_table_clipboard.Rd
  * format_to_regex.Rd
  * formatTimeDiff.Rd
  * length_check.Rd
  * loadAllNamespace.Rd
  * mapply0.Rd
  * null_efault.Rd
  * split_expression.Rd
  * time_inherits.Rd
* These functions were mistakenly flagged as non-exported functions and do not appear in the Rd files noted.  Rd files are removed as per above:
  * diff_time_hours() in: format_to_regex.Rd
  * newest_file() in: mapply0.Rd
  * get_error() in: split_expression.Rd
    
  
  