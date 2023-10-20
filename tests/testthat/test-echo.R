test_that("echo() works", {
  expect_output(
    expect_warning(
      expect_identical(echo({ 1 }), 1),
      regexp = "echo::echo()",
      fixed = TRUE,
      class = "deprecatedWarning"
    )
  )

  expect_output(
    expect_warning(
      echo({ message("m") }, msg = FALSE),
      regexp = "echo::echo()",
      fixed = TRUE,
      class = "deprecatedWarning"
    )
  )

  expect_output(
    expect_error(
      expect_warning(
        echo({
          1 + 1
          Sys.sleep(2)
          head(mtcars)
          message(1)
          warning(2)
          stop(3)
        }),
        regexp = "echo::echo()",
        fixed = TRUE,
        class = "deprecatedWarning"
      ),
      regexp = "3",
      class = "simpleError"
    )
  )
})
