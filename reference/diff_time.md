# Diff time wrappers

Wrappers for computing diff times

## Usage

``` r
diff_time(
  x,
  y,
  method = c("secs", "mins", "hours", "days", "weeks", "months", "years", "dyears",
    "wyears", "myears"),
  tzx = NULL,
  tzy = tzx
)

diff_time_days(x, y, ...)

diff_time_weeks(x, y, ...)

diff_time_hours(x, y, ...)

diff_time_mins(x, y, ...)

diff_time_secs(x, y, ...)

diff_time_months(x, y, ...)

diff_time_years(x, y, ...)

diff_time_dyears(x, y, ...)

diff_time_wyears(x, y, ...)

diff_time_myears(x, y, ...)
```

## Arguments

- x, y:

  Vectors of times

- method:

  A method to report the difference in units of time (see **Units**
  section)

- tzx, tzy:

  time zones (see **Time zones** section)

- ...:

  Additional arguments passed to `diff_time()`

## Value

A `diff_time` vector, object

## Details

A few significant differences exist with these functions \* The class of
the object returned is no longer `difftime` (but does print) with the
`difftime` method. This makes the exporting process easier as the data
will not have to be converted back to `numeric` \*
[`difftime()`](https://rdrr.io/r/base/difftime.html) computes the
difference of `time1` - `time2`, but the inverse feels a bit more
nature: time difference from `x` to `y` \* Additional units can be used
(detailed below) \* Differences can be sensitive to time zones if time
zones are passed to the `tz` parameter as a character vector

## Units

Units can be used beyond those available in
[`base::difftime()`](https://rdrr.io/r/base/difftime.html). Some of
these use assumptions in how units of time should be standardized and
can be changed in the corresponding options. Any of these can be
calculated with
[`base::difftime()`](https://rdrr.io/r/base/difftime.html) through using
`units = "days"` but the `dtime` class will print out with these
specifications into the console for less potential confusion.

- months:

  Months by number of days `mark.days_in_month` (defaults: `30`)

- years:

  Years by number of days `mark.days_in_year` (defaults: `365`)

- dyears:

  Years by number of days `mark.days_in_year` (defaults: `365`) (same as
  `years`)

- myears:

  Years by number of days in a month `mark.days_in_month` (defaults:
  `30`)

- wyears:

  Years by number of weeks in a year `mark.weeks_in_year` (defaults:
  `52`)

## Time zones

Time zones can be passed as either a numeric vector of GMT/UTC offsets
(the number of seconds from GMT) or as a character vector. If the
letter, these need to conform with values from
[`base::OlsonNames()`](https://rdrr.io/r/base/timezones.html).

A default timezone can be set with `options(mark.default_tz = .)`. The
value can either be a numeric
