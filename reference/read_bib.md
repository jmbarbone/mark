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
  [`readLines()`](https://rdrr.io/r/base/readLines.html)

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

if (package_available("tibble")) {
  tibble::as_tibble(bibdf)
} else {
  head(bibdf)
}
#> # A tibble: 4 × 16
#>   key     field author journal title year  issn  month number pages volume doi  
#>   <chr>   <chr> <chr>  <chr>   <chr> <chr> <chr> <chr> <chr>  <chr> <chr>  <chr>
#> 1 ames20… arti… Ames,… Journa… The … 2006  0092… aug   4      440-… 40     10.1…
#> 2 anders… arti… Ander… Psycho… Effe… 2001  NA    NA    5      353-… 12     10.1…
#> 3 ayduk2… arti… Ayduk… Journa… Regu… 2000  NA    NA    5      776   79     10.1…
#> 4 barbon… inpr… Barbo… NA      Equa… 2023  NA    Jul   S18    e077… 19     doi/…
#> # ℹ 4 more variables: publisher <chr>, note <chr>, url <chr>, abstract <chr>

if (package_available("bib2df") & package_available("bench")) {
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
#> 1 read_bib        2ms 2.07ms      478.    9.41KB     6.52   220     3      460ms
#> 2 bib2df       5.87ms 6.05ms      164.    2.83MB     6.65    74     3      451ms
#> 3 foo          2.73ms 2.81ms      350.   348.1KB     4.24   165     2      471ms
```
