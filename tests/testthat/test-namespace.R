
test_that("loadAllNamespace() works", {
  expect_message(loadAllNamespace("magrittr", warn.conflicts = FALSE), NA)
  detach("package:magrittr")
  # should throw at least 1 message
  muffle(expect_message(loadAllNamespace("magrittr")))
  detach("package:magrittr")
})

# lintr thinks there isn't a terminal line here?
