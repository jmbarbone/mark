
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jordan

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/jmbarbone/jordan.svg?branch=master)](https://travis-ci.com/jmbarbone/jordan)
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
#>  [1] "Lorem ipsum dolor sit amet, id maecenas luctus ame"
#>  [2] "t ipsum nibh laoreet faucibus donec at, venenatis."
#>  [3] " Consequat luctus maximus ut, mi malesuada non qua"
#>  [4] "m. Fusce aliquam et pellentesque per, eros tincidu"
#>  [5] "nt non. Donec dis gravida non integer cubilia at, "
#>  [6] "ipsum fringilla congue vivamus. Dui sed maximus a "
#>  [7] "velit cras sem enim placerat lobortis, sed purus t"
#>  [8] "incidunt fermentum. Nullam per fermentum facilisis"
#>  [9] " ultricies urna. Urna, ultrices sed proin potenti "
#> [10] "ipsum ac risus, luctus faucibus gravida congue ut "
#> [11] "cum semper. Ut parturient tristique nec congue tac"
#> [12] "iti malesuada sed! Aptent tortor. Tempus ac aliqua"
#> [13] "m eget erat sed in in congue vestibulum. Ac pellen"
#> [14] "tesque potenti nec vitae convallis integer sed sed"
#> [15] " blandit amet taciti nascetur. Himenaeos nunc sed "
#> [16] "odio scelerisque diam, molestie."
str_slice_by_word(x)
#>  [1] "Lorem ipsum dolor sit amet, id maecenas luctus amet ipsum nibh laoreet faucibus"
#>  [2] " donec at, venenatis. Consequat luctus maximus ut, mi malesuada non quam. Fusce"
#>  [3] " aliquam et pellentesque per, eros tincidunt non. Donec dis gravida non integer"
#>  [4] " cubilia at, ipsum fringilla congue vivamus. Dui sed maximus a velit cras sem"  
#>  [5] " enim placerat lobortis, sed purus tincidunt fermentum. Nullam per fermentum"   
#>  [6] " facilisis ultricies urna. Urna, ultrices sed proin potenti ipsum ac risus,"    
#>  [7] " luctus faucibus gravida congue ut cum semper. Ut parturient tristique nec"     
#>  [8] " congue taciti malesuada sed! Aptent tortor. Tempus ac aliquam eget erat sed in"
#>  [9] " in congue vestibulum. Ac pellentesque potenti nec vitae convallis integer sed" 
#> [10] " sed blandit amet taciti nascetur. Himenaeos nunc sed odio scelerisque diam,"   
#> [11] " molestie."
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
