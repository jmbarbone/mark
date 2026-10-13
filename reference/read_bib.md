# Read Bib file

Read a bib file into a data.frame

## Usage

``` r
read_bib(file, skip = 0L, max_lines = NULL, encoding = "UTF-8")
```

## Arguments

- file:

  File or connection

- skip:

  The lines to skip

- max_lines:

  The maximum number of lines to read

- encoding:

  Assumed encoding of file (passed to
  [`base::readLines()`](https://rdrr.io/r/base/readLines.html)

## Value

A `data.frame` with each row as a bib entry and each column as a field

## Details

Inspired and partially credited to
[`bib2df::bib2df()`](https://docs.ropensci.org/bib2df/reference/bib2df.html)
although this has no dependencies outside of base functions and much
quicker. This speed seems to come from removing `stringr` functions and
simplifying a few \*apply functions. This will also include as many
categories as possible from the entry.

## See also

[`?bib2df::bib2df`](https://docs.ropensci.org/bib2df/reference/bib2df.html)

## Examples

``` r
file <- "https://raw.githubusercontent.com/jmbarbone/bib-references/master/references.bib"
bibdf <- read_bib(file, max_lines = 51L)

if (fuj::available_namespace("tibble")) {
  tibble::as_tibble(bibdf)
} else {
  head(bibdf)
}
#> # A tibble: 6 × 18
#>   key   field title year  url   institution file  publisher urldate websitetitle
#>   <chr> <chr> <chr> <chr> <chr> <chr>       <chr> <chr>     <chr>   <chr>       
#> 1 2017… tech… "201… 2017  http… Neilsen     ":20… NA        NA      NA          
#> 2 2017… tech… "Buz… 2017  http… BuzzAngle …  NA   NA        NA      NA          
#> 3 acad… webs… "Aca… NA    http… NA           NA   Johns Ho… 2018-1… Data Manage…
#> 4 adam… book  "The… 1989  NA    NA           NA   Harmony … NA      NA          
#> 5 ali2… arti… "Int… 2010  NA    NA           NA   Universi… NA      NA          
#> 6 ames… arti… "The… 2006  NA    NA           NA   Elsevier  NA      NA          
#> # ℹ 8 more variables: author <chr>, address <chr>, isbn <chr>, journal <chr>,
#> #   volume <chr>, number <chr>, pages <chr>, doi <chr>

if (fuj::available_namespace("bib2df") & fuj::available_namespace("bench")) {
  file <- system.file("extdata", "bib2df_testfile_3.bib", package = "bib2df")

  # Doesn't include the 'tidying' up
  foo <- function(file) {
    bib <- ("bib2df" %colons% "bib2df_read")(file)
    ("bib2df" %colons% "bib2df_gather")(bib)
  }

# \donttest{
  bench::mark(
    read_bib = read_bib(file),
    bib2df = bib2df::bib2df(file),
    foo = foo(file),
    check = FALSE
  )[1:9]
# }
}
#> # A tibble: 3 × 9
#>   expression      min median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time
#>   <bch:expr> <bch:tm> <bch:>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm>
#> 1 read_bib     1.81ms 1.92ms      516.    9.41KB     9.06   228     4      442ms
#> 2 bib2df       5.76ms 6.05ms      160.     8.6MB     4.33    74     2      462ms
#> 3 foo          3.29ms 3.47ms      283.  385.74KB     4.28   132     2      467ms
```
