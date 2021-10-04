test_that("depth", {
  expect_equal(depth(NA), 1L)
  expect_equal(depth(data.frame(a = 1)), 1L)
  expect_equal(depth(list()), 0L)
  expect_equal(depth(NULL), 0L)
  expect_equal(depth(list(1)), 1L)
  expect_equal(depth(list(list())), 1L)
  expect_equal(depth(list(list(1))), 2L)
  expect_equal(depth(c(1, 2, 3)), 1L)
  expect_equal(depth(list(list(1, list(list())))), 3L)

  ls <- list(a = 1,
             b = list(
               # 2
               list(
                 3
               )
             )
  )
  expect_equal(depth(ls), 3L)

  ls <- list(
    a = 1,
    b = 1,
    c = list(
      a = 2,
      b = list(
        # 3
        a = list(
          a = 4
        ),
        b = 3
      )
    ),
    d = 1,
    e = 1
  )
  expect_equal(depth(ls), 4L)
})
