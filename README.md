
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jordan

<!-- badges: start -->
<!-- badges: end -->

**j**ust **o**ther **R** **d**ata **a**nalytic **n**uggets

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
#> [1] "Lorem ipsum dolor sit amet, nullam in eu torquent."
#> [2] " Phasellus sociis, aptent, sociis id vel finibus. "
#> [3] "Elit, faucibus erat nulla fames libero consequat, "
#> [4] "mi cursus. Vehicula augue sed non dui. Porta ut to"
#> [5] "rtor nec in litora sed non odio. Tellus ullamcorpe"
#> [6] "r vulputate dictum ut nostra. Urna sodales mauris "
#> [7] "malesuada cum vitae, a, maecenas. Sit accumsan, at"
#> [8] ", tellus ridiculus egestas litora."
str_slice_by_word(x)
#> [1] "Lorem ipsum dolor sit amet, nullam in eu torquent. Phasellus sociis, aptent,"    
#> [2] " sociis id vel finibus. Elit, faucibus erat nulla fames libero consequat, mi"    
#> [3] " cursus. Vehicula augue sed non dui. Porta ut tortor nec in litora sed non odio."
#> [4] " Tellus ullamcorper vulputate dictum ut nostra. Urna sodales mauris malesuada"   
#> [5] " cum vitae, a, maecenas. Sit accumsan, at, tellus ridiculus egestas litora."
```

Read in bibliographies:

``` r
file <- "https://raw.githubusercontent.com/jmbarbone/bib-references/master/references.bib"
bib <- read_bib(file)
tibble::as_tibble(bib)
#> # A tibble: 137 x 33
#>    key   field author journal title year  issn  month number pages volume doi  
#>    <chr> <chr> <chr>  <chr>   <chr> <chr> <chr> <chr> <chr>  <chr> <chr>  <chr>
#>  1 ames~ arti~ Ames,~ Journa~ "The~ 2006  0092~ aug   4      440-~ 40     10.1~
#>  2 ande~ arti~ Ander~ Psycho~ "Eff~ 2001  <NA>  <NA>  5      353-~ 12     10.1~
#>  3 aydu~ arti~ Ayduk~ Journa~ "Reg~ 2000  <NA>  <NA>  5      776   79     10.1~
#>  4 bake~ arti~ Baker~ Nature~ "1,5~ 2016  <NA>  <NA>  7604   452   533    10.1~
#>  5 begl~ arti~ Begle~ Circul~ "Rep~ 2015  <NA>  <NA>  1      116-~ 116    10.1~
#>  6 benj~ arti~ Benja~ Curren~ "RET~ 2018  2352~ <NA>  <NA>   93--~ 19     10.1~
#>  7 benj~ arti~ Benja~ Person~ "Eff~ 2018  <NA>  <NA>  4      347-~ 22     10.1~
#>  8 brau~ book  Braud~ <NA>    "ESP~ 2002  <NA>  <NA>  <NA>   <NA>  <NA>   <NA> 
#>  9 burg~ arti~ Burge~ Motiva~ "The~ 1979  <NA>  <NA>  4      381-~ 3      10.1~
#> 10 bush~ arti~ Bushm~ Aggres~ "{\\~ 2018  <NA>  sep   1      33--~ 45     10.1~
#> # ... with 127 more rows, and 21 more variables: publisher <chr>, note <chr>,
#> #   abstract <chr>, eprint <chr>, isbn <chr>, url <chr>, keywords <chr>,
#> #   websitetitle <chr>, urldate <chr>, organization <chr>, address <chr>,
#> #   language <chr>, day <chr>, retraction <chr>, school <chr>,
#> #   institution <chr>, howpublished <chr>, comment <chr>, event <chr>,
#> #   booktitle <chr>, conference <chr>
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
