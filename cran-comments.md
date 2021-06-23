## Test environments
* local R installation, R 4.1.0
* windows-latest, release 4.0, 3.6
* macOS-latest, release, 4.0, 3.6
* ubuntu 20.04, release, 4.0, 3.6

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a patch release

* Removes created temp files:
  * Example in: add_file_timestamp.Rd
  * Example in: eval_named_chunk.Rd
  * In test: test-sourcing.Rmd
  * Removes temporary files when running source_to_env()
  * Simplified temp files/directories using tools::R_user_home() and tempdir()
  