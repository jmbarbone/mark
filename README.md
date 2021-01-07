
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jordan

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/jmbarbone/jordan.svg?branch=master)](https://travis-ci.com/jmbarbone/jordan)
<!-- badges: end -->

Just Other R Data Analytic Nuggets

An R package with a set of general use functions for data analytics.
This is developed mostly for personal use and has no real *goal* other
than to limit the time I spend searching where I did that thing that I
think I could use again because it worked well but this problem might be
slightly different and I know I had to change it before.

Some parts happily ripped from and (hopefully) credited to others.

## Installation

You can the development version from
[GitHub](https://github.com/jmbarbone/jordan) with:

    devtools::install_github("jmbarbone/jordan")

## Select examples

This package contains a many variety of functions, some useful, some not
so much. Below are a selection of a few functions that could potential
be useful for others:

``` r
library(jordan)
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
#>  [1] "Lorem ipsum dolor sit amet, bibendum ipsum rutrum "
#>  [2] "primis nisl. Ligula commodo lorem ex laoreet, adip"
#>  [3] "iscing fermentum turpis, eget. Id, ut eget odio ve"
#>  [4] "stibulum interdum aptent et. Ac porttitor nostra m"
#>  [5] "i. Sed habitant, fringilla pellentesque at egestas"
#>  [6] " lobortis risus pellentesque quisque libero proin."
#>  [7] " Netus euismod pharetra sed, fusce tristique phase"
#>  [8] "llus purus, conubia id. Cras iaculis per potenti i"
#>  [9] "mperdiet auctor non et gravida posuere, leo venena"
#> [10] "tis! Et eu velit est vestibulum, quis, ac sed ex, "
#> [11] "lacus, mollis. Tristique in sed fusce et molestie "
#> [12] "amet mauris! Sapien pellentesque dapibus in velit "
#> [13] "accumsan. In mauris ligula volutpat nunc nec susci"
#> [14] "pit eu. Nibh ante, ipsum pulvinar in nulla est et "
#> [15] "cubilia donec primis enim dapibus ut montes eget. "
#> [16] "Cum vestibulum nascetur eros ultricies at. Semper "
#> [17] "augue ipsum hac dui."
str_slice_by_word(x)
#>  [1] "Lorem ipsum dolor sit amet, bibendum ipsum rutrum primis nisl. Ligula commodo"   
#>  [2] " lorem ex laoreet, adipiscing fermentum turpis, eget. Id, ut eget odio"          
#>  [3] " vestibulum interdum aptent et. Ac porttitor nostra mi. Sed habitant, fringilla" 
#>  [4] " pellentesque at egestas lobortis risus pellentesque quisque libero proin. Netus"
#>  [5] " euismod pharetra sed, fusce tristique phasellus purus, conubia id. Cras iaculis"
#>  [6] " per potenti imperdiet auctor non et gravida posuere, leo venenatis! Et eu velit"
#>  [7] " est vestibulum, quis, ac sed ex, lacus, mollis. Tristique in sed fusce et"      
#>  [8] " molestie amet mauris! Sapien pellentesque dapibus in velit accumsan. In mauris" 
#>  [9] " ligula volutpat nunc nec suscipit eu. Nibh ante, ipsum pulvinar in nulla est et"
#> [10] " cubilia donec primis enim dapibus ut montes eget. Cum vestibulum nascetur eros" 
#> [11] " ultricies at. Semper augue ipsum hac dui."
```

Read in bibliographies:

``` r
file <- system.file("extdata", "example-bib.txt", package = "jordan")
bib <- read_bib(file)
tibble::as_tibble(bib)
#> # A tibble: 13 x 23
#>    key   field author title journal year  number pages month note  volume
#>    <chr> <chr> <chr>  <chr> <chr>   <chr> <chr>  <chr> <chr> <chr> <chr> 
#>  1 arti~ arti~ Peter~ The ~ The na~ 1993  2      201-~ 7     An o~ 4     
#>  2 book  book  Peter~ The ~ <NA>    1993  <NA>   <NA>  7     An o~ 4     
#>  3 book~ book~ Peter~ The ~ <NA>    1993  <NA>   <NA>  7     An o~ <NA>  
#>  4 conf~ conf~ Peter~ The ~ <NA>    1993  <NA>   213   7     An o~ 4     
#>  5 inbo~ inbo~ Peter~ The ~ <NA>    1993  <NA>   201-~ 7     An o~ 4     
#>  6 inco~ inco~ Peter~ The ~ <NA>    1993  <NA>   201-~ 7     An o~ 4     
#>  7 manu~ manu~ Peter~ The ~ <NA>    1993  <NA>   <NA>  7     An o~ <NA>  
#>  8 mast~ mast~ Peter~ The ~ <NA>    1993  <NA>   <NA>  7     An o~ <NA>  
#>  9 misc  misc  Peter~ The ~ <NA>    1993  <NA>   <NA>  7     An o~ <NA>  
#> 10 phdt~ phdt~ Peter~ The ~ <NA>    1993  <NA>   <NA>  7     An o~ <NA>  
#> 11 proc~ proc~ <NA>   The ~ <NA>    1993  <NA>   <NA>  7     An o~ 4     
#> 12 tech~ tech~ Peter~ The ~ <NA>    1993  2      <NA>  7     An o~ <NA>  
#> 13 unpu~ unpu~ Peter~ The ~ <NA>    1993  <NA>   <NA>  7     An o~ <NA>  
#> # ... with 12 more variables: publisher <chr>, series <chr>, address <chr>,
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
