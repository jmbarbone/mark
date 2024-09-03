test_that("eval_named_chunk()", {
  temp_rmd <- mark_temp(".rmd")

  text <- '
  ```{r not this label}
  print("that is wrong")
  ```

  ```{r hello label}
  text <- "hello, world"
  print(text)
  print(TRUE)
  ```

  ```{r another label}
  warning("wrong label")
  ```
  '

  writeLines(text, con = temp_rmd)
  expect_output(
    eval_named_chunk(temp_rmd, "hello label"),
    '\\[1\\] "hello, world"\n\\[1\\] TRUE'
  )
  file.remove(temp_rmd)

  expect_error(eval_named_chunk(tempfile()), class = "evalNamedChunkRmdError")
  file <- tempfile(fileext = ".Rmd")
  file.create(file)
  expect_error(eval_named_chunk(file), "\"label_name\" is missing")
  file.remove(file)
})

test_that("Rscript", {
  x <- test_path("scripts", "rscript-test.R")
  expect_error(rscript(x, "vanilla", stdout = FALSE, stderr = FALSE), NA)
  expect_false("dplyr" %in% search())

  expect_error(rscript(x, stdout = FALSE, stderr = FALSE), NA)
  expect_false("dplyr" %in% search())

  e <- source_to_env(x, ops = "vanilla")
  expect_s3_class(e, c("source_env", "environment"))
  expect_identical(e$a_litte_note, "You're doing okay")
  expect_s3_class(e$out, "tbl_df")
})
