
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mark

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/mark)](https://CRAN.R-project.org/package=mark)
[![R-CMD-check](https://github.com/jmbarbone/mark/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jmbarbone/mark/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jmbarbone/mark/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jmbarbone/mark?branch=main)
<!-- badges: end -->

Miscellaneous, Analytic R Kernels

An R package with a set of general use functions for data analytics.
This is developed mostly for personal use and has no real *goal* other
than to limit the time I spend searching where I did that thing that I
think I could use again because it worked well but this problem might be
slightly different and I know I had to change it before.

Some parts happily ripped from and (hopefully) credited to others.

## Installation

You can download the current CRAN version with:

``` r
install.packages("mark")
```

You can the development version from
[GitHub](https://github.com/jmbarbone/mark) with:

``` r
remotes::install_github("jmbarbone/mark")
```

## Select examples

This package contains a many variety of functions, some useful, some not
so much. Below are a selection of a few functions that could potential
be useful for others:

``` r
library(mark)
#> 
#> Attaching package: 'mark'
#> The following object is masked from 'package:usethis':
#> 
#>     use_author
```

Get dates from sloppy entries:

``` r
bad_dates <- c("2020 Dec 8th", "1970 May", "??", "1984 UNK UN")
date_from_partial(bad_dates)
#> [1] "2020-12-08" "1970-05-01" NA           "1984-01-01"
date_from_partial(bad_dates, method = "max")
#> [1] "2020-12-08" "1970-05-31" NA           "1984-12-31"
date_from_partial(c("May 2000", "08Dec2020"), format = "dmy")
#> [1] "2000-05-01" "2020-12-08"
```

Slice strings:

``` r
x <- stringi::stri_rand_lipsum(1)
str_slice(x, n = 50L)
#>  [1] "Lorem ipsum dolor sit amet, nisl eleifend sed proi"
#>  [2] "n sed at. Class maximus, ante mi sed ridiculus eni"
#>  [3] "m mus, sollicitudin. Maecenas penatibus luctus don"
#>  [4] "ec turpis erat pretium in vulputate accumsan. Amet"
#>  [5] " quis arcu phasellus facilisi facilisis odio integ"
#>  [6] "er sit. Nunc venenatis duis vitae in non mauris ri"
#>  [7] "sus. Vel consectetur sed sapien arcu sed massa nec"
#>  [8] " egestas, malesuada condimentum felis a? Et ut pel"
#>  [9] "lentesque consequat sed at torquent, sociosqu. Sod"
#> [10] "ales donec arcu laoreet luctus auctor mauris mauri"
#> [11] "s nisl primis nascetur feugiat scelerisque libero."
#> [12] " Sed maximus vehicula dictum lacus libero pharetra"
#> [13] " sed. Egestas maximus venenatis egestas leo orci, "
#> [14] "tellus consectetur velit litora nascetur, a. Ferme"
#> [15] "ntum aptent lobortis elementum netus integer variu"
#> [16] "s euismod ac ornare porttitor non ut quam, mollis."
#> [17] " Scelerisque cursus amet primis. Vestibulum non co"
#> [18] "nsectetur aliquam mollis velit accumsan. Condiment"
#> [19] "um sit sed eu dapibus habitant faucibus interdum. "
#> [20] "Vel libero, amet lacus aliquam ac sit porta, leo l"
#> [21] "eo."
str_slice_by_word(x)
#>  [1] "Lorem ipsum dolor sit amet, nisl eleifend sed proin sed at. Class maximus, ante" 
#>  [2] "mi sed ridiculus enim mus, sollicitudin. Maecenas penatibus luctus donec turpis" 
#>  [3] "erat pretium in vulputate accumsan. Amet quis arcu phasellus facilisi facilisis" 
#>  [4] "odio integer sit. Nunc venenatis duis vitae in non mauris risus. Vel consectetur"
#>  [5] "sed sapien arcu sed massa nec egestas, malesuada condimentum felis a? Et ut"     
#>  [6] "pellentesque consequat sed at torquent, sociosqu. Sodales donec arcu laoreet"    
#>  [7] "luctus auctor mauris mauris nisl primis nascetur feugiat scelerisque libero. Sed"
#>  [8] "maximus vehicula dictum lacus libero pharetra sed. Egestas maximus venenatis"    
#>  [9] "egestas leo orci, tellus consectetur velit litora nascetur, a. Fermentum aptent" 
#> [10] "lobortis elementum netus integer varius euismod ac ornare porttitor non ut quam,"
#> [11] "mollis. Scelerisque cursus amet primis. Vestibulum non consectetur aliquam"      
#> [12] "mollis velit accumsan. Condimentum sit sed eu dapibus habitant faucibus"         
#> [13] "interdum. Vel libero, amet lacus aliquam ac sit porta, leo leo."
```

Read in bibliographies:

``` r
file <- system.file("extdata", "example-bib.txt", package = "mark")
bib <- read_bib(file)
tibble::as_tibble(bib)
#> # A tibble: 13 × 23
#>    key          field author title journal year  number pages month note  volume
#>    <chr>        <chr> <chr>  <chr> <chr>   <chr> <chr>  <chr> <chr> <chr> <chr> 
#>  1 article      arti… Peter… The … The na… 1993  2      201-… 7     An o… 4     
#>  2 book         book  Peter… The … <NA>    1993  <NA>   <NA>  7     An o… 4     
#>  3 booklet      book… Peter… The … <NA>    1993  <NA>   <NA>  7     An o… <NA>  
#>  4 conference   conf… Peter… The … <NA>    1993  <NA>   213   7     An o… 4     
#>  5 inbook       inbo… Peter… The … <NA>    1993  <NA>   201-… 7     An o… 4     
#>  6 incollection inco… Peter… The … <NA>    1993  <NA>   201-… 7     An o… 4     
#>  7 manual       manu… Peter… The … <NA>    1993  <NA>   <NA>  7     An o… <NA>  
#>  8 mastersthes… mast… Peter… The … <NA>    1993  <NA>   <NA>  7     An o… <NA>  
#>  9 misc         misc  Peter… The … <NA>    1993  <NA>   <NA>  7     An o… <NA>  
#> 10 phdthesis    phdt… Peter… The … <NA>    1993  <NA>   <NA>  7     An o… <NA>  
#> 11 proceedings  proc… <NA>   The … <NA>    1993  <NA>   <NA>  7     An o… 4     
#> 12 techreport   tech… Peter… The … <NA>    1993  2      <NA>  7     An o… <NA>  
#> 13 unpublished  unpu… Peter… The … <NA>    1993  <NA>   <NA>  7     An o… <NA>  
#> # ℹ 12 more variables: publisher <chr>, series <chr>, address <chr>,
#> #   edition <chr>, isbn <chr>, howpublished <chr>, booktitle <chr>,
#> #   editor <chr>, organization <chr>, chapter <chr>, school <chr>,
#> #   institution <chr>
```

More matching:

``` r
1:10 %out% c(1, 3, 5, 9) # opposite of %in% 
#>  [1] FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE
letters[1:5] %wo% letters[3:7]
#> [1] "a" "b"
letters[1:5] %wi% letters[3:7]
#> [1] "c" "d" "e"
```

Small functions for working with data.frames:

``` r
x <- list(a = 1:5, b = letters[1:5])
quick_df(x)
#>   a b
#> 1 1 a
#> 2 2 b
#> 3 3 c
#> 4 4 d
#> 5 5 e

vector2df(x[["b"]], name = NULL)
#>   value
#> 1     a
#> 2     b
#> 3     c
#> 4     d
#> 5     e

quick_dfl(a = 1:3, b = list(1:5, 6:10, 11:15))
#>   a                  b
#> 1 1      1, 2, 3, 4, 5
#> 2 2     6, 7, 8, 9, 10
#> 3 3 11, 12, 13, 14, 15
```

Counts and proportions:

``` r
set.seed(42)
x <- sample(1:5, 20, TRUE, 5:1/2)
counts(x)
#> 4 5 1 3 2 
#> 2 4 4 5 5
props(x)
#>    4    5    1    3    2 
#> 0.10 0.20 0.20 0.25 0.25

df <- as.data.frame(matrix(sample(1:2, 60, TRUE), byrow = TRUE, ncol = 3))
counts(df, c("V1", "V2"))
#>   V1 V2 freq
#> 1  1  1    5
#> 2  1  2    4
#> 3  2  2    8
#> 4  2  1    3
props(df, 1:3)
#>   V1 V2 V3      prop
#> 1  1  1  1 0.4285714
#> 2  1  1  2 0.2857143
#> 3  1  2  2 0.4285714
#> 4  2  2  1 0.7142857
#> 5  2  1  2 0.4285714
#> 6  2  2  2 0.4285714
#> 7  1  2  1 0.1428571
```

Date time differences:

``` r
x <- as.POSIXlt("2021-02-13 05:02:30", tz = "America/New_York") + c(0, -1, 2) * 3600 * 24
y <- as.POSIXlt("2020-02-13 05:02:30", tz = "America/New_York") + c(0, -2, 4) * 3600 * 24

# comparison with base::difftime() (note the order of x and y)
difftime(y, x, units = "days")
#> Time differences in days
#> [1] -366 -367 -364
diff_time_days(x, y)
#> Time differences in days
#> [1] -366 -367 -364

difftime(y, x, units = "secs")
#> Time differences in secs
#> [1] -31622400 -31708800 -31449600
diff_time_secs(x, y)
#> Time differences in seconds
#> [1] -31622400 -31708800 -31449600

# Year (by days, months, etc)
diff_time_years(x, y)
#> Time differences in years (365 days)
#> [1] -1.0027397 -1.0054795 -0.9972603
diff_time_myears(x, y)
#> Time differences in years (30-day months)
#> [1] -1.016667 -1.019444 -1.011111

# Set time zones
diff_time_hours(x, y, "GMT", "America/New_York")                         
#> Time differences in hours
#> [1] -8789 -8813 -8741
diff_time_hours(x, x, "GMT", c("America/Los_Angeles", "America/New_York", "Europe/London")) # note x, x
#> Time differences in hours
#> [1] -8 -5  0
diff_time_days(x, y, NULL, 31536000) 
#> Time differences in days
#> [1] -0.994213 -1.994213  1.005787
```

Simple factors:

``` r
fact(c("a", "c", NA, "a", "b", NA, "a", "c")) # no sorting
#> [1] a    c    <NA> a    b    <NA> a    c   
#> Levels: a c b <NA>
fact(c(-1, 5, 2, NA, 3))                      # sorting
#> [1] -1   5    2    <NA> 3   
#> Levels: -1 2 3 5 <NA>
fact(c(NA, FALSE, TRUE, FALSE, TRUE, NA))     # fixed
#> [1] <NA>  FALSE TRUE  FALSE TRUE  <NA> 
#> Levels: TRUE FALSE <NA>
```
