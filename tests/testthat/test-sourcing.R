test_that("eval_named_chunk()", {
  temp_rmd <- tempfile(fileext = ".rmd")
  on.exit(file.remove(temp_rmd), add = TRUE)

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
})
