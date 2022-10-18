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
    a = st + seq.int(1, by = 3600 * 24 * 5, length.out = n),
    b = st - seq.int(1, by = 3600 * 24 * 5, length.out = n),
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

  dftz <- quick_dfl(
    a = rep(st, 4),
    b = rep(st, 4),
    tza = c("GMT", "UTC", "US/Eastern", "NZ"),
    tzb = c("GMT", "Africa/Casablanca", "US/Central", "CET"),
    tzn = c(0, 1, -1, 6) * 3600
  )

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
    as.double(with(dftz, diff_time_years(a, b, tza, tzb))),
    as.double(with(dftz, diff_time_dyears(a, b, tza, tzb)))
  )

  expect_equal(
    as.double(with(dftz, diff_time_dyears(a, b, tza, tzb))),
    as.double(with(dftz, diff_time_days(a, b, tza, tzb)) / 365)
  )

  expect_equal(
    as.double(with(dftz, diff_time_wyears(a, b, tza, tzb))),
    as.double(with(dftz, diff_time_weeks(a, b, tza, tzb)) / 52)
  )

  expect_equal(
    as.double(with(dftz, diff_time_myears(a, b, tza, tzb))),
    as.double(with(dftz, diff_time_months(a, b, tza, tzb)) / 12)
  )

  expect_equal(
    as.double(with(dftz, diff_time_years(a, b))),
    as.double(with(dftz, diff_time_dyears(a, b)))
  )

  expect_equal(
    with(dftz, diff_time_hours(a, b)),
    with(dftz, diff_time_hours(a, b, -3600, -3600))
  )

  expect_equal(
    as.double(with(dftz, diff_time_months(a, b, tza, tzb))),
    as.double(with(dftz, diff_time_days(a, b, tza, tzb)) / 30)
  )

  # Off sets are by the hour
  expect_equal(
    with(dftz, diff_time_hours(a, b, tzn, NULL)),
    c(0.14, -1.14, 0.86, -6.14),
    tolerance = 0.035,
    ignore_attr = TRUE
  )

  expect_equal(
    with(dftz, diff_time_hours(a, b, 0:3 * 3600, NULL)),
    -c(0.14, 1.14, 2.14, 3.14),
    tolerance = 0.01,
    ignore_attr = TRUE
  )

  # Reversed -- skipping tza
  expect_equal(
    with(dftz, diff_time_hours(a, b, NULL, 0:3 * 3600)),
    c(0.14, 1.14, 2.14, 3.14),
    tolerance = 0.01,
    ignore_attr = TRUE
  )

  expect_identical(
    with(dftz, diff_time_hours(a, b, tza, tzb)),
    c(0, 1, -1, -10),
    ignore_attr = TRUE
  )

  expect_warning(
    diff_time(Sys.Date(), Sys.Date(), tzx = NA, tzy = "GMT"),
    "NA found in timezones"
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

test_that("class coehersion", {

  expect_identical(
    diff_time(as.Date("2021-07-26"), "2021-07-26"),
    diff_time(as.Date("2021-07-26"), as.Date("2021-07-26"))
  )

  expect_identical(
    diff_time(as.Date("2021-07-26"), "2021-07-26"),
    # setting to date instead
    diff_time(as.Date("2021-07-26"), as.POSIXct("2021-07-26 02:02:02"))
  )

  expect_warning(
    to_numeric_with_tz("2021-01-01", NA),
    "NA found in timezones"
  )

  expect_identical(check_tz(NULL), NULL)
  expect_identical(check_tz(c("UTC", "UTC")), NULL)
})

test_that("class cohersion <4.3.0", {
  skip_if(getRversion() >= "4.3")
  expect_identical(
    extract_numeric_time("2021-01-01", NULL),
    struct(1609459200, "double", tzone = "UTC")
  )

  expect_identical(
    extract_numeric_time(as.POSIXlt("2021-01-01", tz = "UTC"), NULL),
    struct(1609459200, "double", tzone = "UTC")
  )
})

test_that("timezones", {
  withr::with_timezone("Pacific/Auckland", {
    expect_identical(
      diff_time(as.Date("2021-07-26"), "2021-07-26"),
      # setting to date instead
      diff_time(as.Date("2021-07-26"), as.POSIXct("2021-07-26 02:02:02"))
    )
  })
})


# helpers -----------------------------------------------------------------

test_that("helpers", {
  expect_true(is_POSIXct(as.POSIXct("2021-08-24")))
  expect_true(is_POSIXlt(as.POSIXlt("2021-08-24")))
  expect_true(is_diff_time(diff_time(Sys.time(), Sys.time() + 1)))
})

test_that("sys_tz() does not fail", {
  expect_error(sys_tz(1), NA)
  expect_error(sys_tz(2), NA)
  expect_error(sys_tz(3), NA)
  expect_error(sys_tz(4), NA)
  expect_error(sys_tz(5), NA)
  expect_error(sys_tz(6), NA)
  expect_error(sys_tz(7), NA)
})


# printing ----------------------------------------------------------------

test_that("snaps", {
  # skip("not currently testing snaps")

  x <- struct(18842L, "Date")
  y <- x + 100L

  expect_snapshot(diff_time(x, y))
  expect_snapshot(diff_time_days(x, y))
  expect_snapshot(diff_time_dyears(x, y))
  expect_snapshot(diff_time_hours(x, y))
  expect_snapshot(diff_time_mins(x, y))
  expect_snapshot(diff_time_months(x, y))
  expect_snapshot(diff_time_myears(x, y))
  expect_snapshot(diff_time_secs(x, y))
  expect_snapshot(diff_time_weeks(x, y))
  expect_snapshot(diff_time_wyears(x, y))
  expect_snapshot(diff_time_years(x, y))
})