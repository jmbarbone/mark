# Set time to current time GMT
st <- as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "GMT")
n <- 10
tz1 <- c(
  "Africa/Gaborone",
  "America/Indiana/Tell_City",
  "America/Jujuy",
  "America/St_Vincent",
  "Asia/Macau",
  "Asia/Yerevan",
  "Australia/Darwin",
  "Australia/West",
  "Etc/GMT+2",
  "Europe/Nicosia",
  NULL
)

tz2 <- c(
    2,
    3,
    6,
    8,
  -10,
  -11,
   12,
  -13,
   16,
  -17,
  NULL
)

df <- quick_df(
  list(
    a = st + seq.int(0, by = 3600 * 24 * 5, length.out = n),
    b = st - seq.int(0, by = 3600 * 24 * 5, length.out = n),
    tz1 = tz1,
    tz2 = tz2 * 3600
  )
)

test_that("diff_time_*() identical to difftime()", {
  expect_identical(
    with(df, difftime(b, a, units = "secs")),
    with(df, diff_time_secs(a, b)),
    ignore_attr = TRUE
  )

  expect_identical(
    with(df, difftime(b, a, units = "mins")),
    with(df, diff_time_mins(a, b)),
    ignore_attr = TRUE
  )

   expect_identical(
    with(df, difftime(b, a, units = "hours")),
    with(df, diff_time_hours(a, b)),
    ignore_attr = TRUE
  )

  expect_identical(
    with(df, difftime(b, a, units = "days")),
    with(df, diff_time_days(a, b)),
    ignore_attr = TRUE
  )

  expect_identical(
    with(df, difftime(b, a, units = "weeks")),
    with(df, diff_time_weeks(a, b)),
    ignore_attr = TRUE
  )
})

test_that("Timezones", {

  st <- as.POSIXct("2021-04-06 11:12:45", tz = "US/Central")

  dftz <- quick_df(list(
    a = rep(st, 4),
    b = rep(st, 4),
    tza = c("GMT", "UTC", "US/Eastern", "NZ"),
    tzb = c("GMT", "Africa/Casablanca", "US/Central", "CET"),
    tzn = c(0, 1, -1, 6) * 3600
  ))

  # No difference
  expect_identical(
    with(dftz, diff_time_hours(a, b)), rep(0, 4),
    ignore_attr = TRUE
  )

  # Default behavior is to copy tz
  expect_equal(
    with(dftz, diff_time_hours(a, b, tzn)),
    with(dftz, diff_time_hours(a, b))
  )

  expect_equal(
    with(dftz, diff_time_hours(a, b, -3600)),
    with(dftz, diff_time_hours(a, b))
  )

  expect_equal(
    with(dftz, diff_time_hours(a, b)),
    with(dftz, diff_time_hours(a, b, -3600, -3600))
  )

  # Off sets are by the hour
  expect_identical(
    with(dftz, diff_time_hours(a, b, tzn, NULL)),
    c(0, -1, 1, -6),
    ignore_attr = TRUE
  )

  expect_identical(
    with(dftz, diff_time_hours(a, b, 0:3 * 3600, NULL)),
    -c(0, 1, 2, 3),
    ignore_attr = TRUE
  )

  # Reversed -- skipping tza
  expect_identical(
    with(dftz, diff_time_hours(a, b, NULL, 0:3 * 3600)),
    c(0, 1, 2, 3),
    ignore_attr = TRUE
  )

  expect_identical(
    with(dftz, diff_time_hours(a, b, tza, tzb)),
    c(0, 1, -1, -10),
    ignore_attr = TRUE
  )
})

test_that("Error checking", {
  expect_error(diff_time_secs(1:10, 1:10), "Date times cannot be numeric")
  expect_error(diff_time_secs(st, st, "Not good"), "OlsonNames()")

  # Don't throw error because of NA tz
  expect_identical(
    diff_time_secs(st, st, c(1, NA, 2, 3)),
    c(0, NA, 0, 0),
    ignore_attr = TRUE
  )
})
