
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jordan

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/jmbarbone/jordan.svg?branch=main)](https://travis-ci.com/jmbarbone/jordan)
[![R-CMD-check](https://github.com/jmbarbone/jordan/workflows/R-CMD-check/badge.svg)](https://github.com/jmbarbone/jordan/actions)
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

    remotes::install_github("jmbarbone/jordan")

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
#>  [2] " mi sed ridiculus enim mus, sollicitudin. Maecenas penatibus luctus donec turpis"
#>  [3] " erat pretium in vulputate accumsan. Amet quis arcu phasellus facilisi facilisis"
#>  [4] " odio integer sit. Nunc venenatis duis vitae in non mauris risus. Vel"           
#>  [5] " consectetur sed sapien arcu sed massa nec egestas, malesuada condimentum felis" 
#>  [6] " a? Et ut pellentesque consequat sed at torquent, sociosqu. Sodales donec arcu"  
#>  [7] " laoreet luctus auctor mauris mauris nisl primis nascetur feugiat scelerisque"   
#>  [8] " libero. Sed maximus vehicula dictum lacus libero pharetra sed. Egestas maximus" 
#>  [9] " venenatis egestas leo orci, tellus consectetur velit litora nascetur, a."       
#> [10] " Fermentum aptent lobortis elementum netus integer varius euismod ac ornare"     
#> [11] " porttitor non ut quam, mollis. Scelerisque cursus amet primis. Vestibulum non"  
#> [12] " consectetur aliquam mollis velit accumsan. Condimentum sit sed eu dapibus"      
#> [13] " habitant faucibus interdum. Vel libero, amet lacus aliquam ac sit porta, leo"   
#> [14] " leo."
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

vector2df(x[["b"]])
#>   name value
#> 1   NA     a
#> 2   NA     b
#> 3   NA     c
#> 4   NA     d
#> 5   NA     e
```

Counting and proportions;

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
#>   V1 V2 V3 prop
#> 1  1  1  1 0.15
#> 2  1  1  2 0.10
#> 3  1  2  2 0.15
#> 4  2  2  1 0.25
#> 5  2  1  2 0.15
#> 6  2  2  2 0.15
#> 7  1  2  1 0.05
```
