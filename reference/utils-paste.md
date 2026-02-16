# Paste combine

Paste and combine

## Usage

``` r
paste_c(x, y, collate = TRUE, sep = "")

paste_combine(..., collate = TRUE, sep = "")

collapse0(..., sep = "")
```

## Arguments

- x, y, ...:

  Vectors to paste and/or combine

- collate:

  Logical; `TRUE` prints out combinations in order of the first vector
  elements then the next; otherwise reversed (see examples)

- sep:

  A character string to separate terms

## Value

A `character` vector

## Examples

``` r
x <- letters[1:5]
y <- 1:3
z <- month.abb[c(1, 12)]
paste_combine(x, y)
#>  [1] "a1" "a2" "a3" "b1" "b2" "b3" "c1" "c2" "c3" "d1" "d2" "d3" "e1" "e2" "e3"
paste_combine(x, y, z)
#>  [1] "a1Jan" "a1Dec" "a2Jan" "a2Dec" "a3Jan" "a3Dec" "b1Jan" "b1Dec" "b2Jan"
#> [10] "b2Dec" "b3Jan" "b3Dec" "c1Jan" "c1Dec" "c2Jan" "c2Dec" "c3Jan" "c3Dec"
#> [19] "d1Jan" "d1Dec" "d2Jan" "d2Dec" "d3Jan" "d3Dec" "e1Jan" "e1Dec" "e2Jan"
#> [28] "e2Dec" "e3Jan" "e3Dec"
paste_combine(x, y, z, sep = ".")
#>  [1] "a.1.Jan" "a.1.Dec" "a.2.Jan" "a.2.Dec" "a.3.Jan" "a.3.Dec" "b.1.Jan"
#>  [8] "b.1.Dec" "b.2.Jan" "b.2.Dec" "b.3.Jan" "b.3.Dec" "c.1.Jan" "c.1.Dec"
#> [15] "c.2.Jan" "c.2.Dec" "c.3.Jan" "c.3.Dec" "d.1.Jan" "d.1.Dec" "d.2.Jan"
#> [22] "d.2.Dec" "d.3.Jan" "d.3.Dec" "e.1.Jan" "e.1.Dec" "e.2.Jan" "e.2.Dec"
#> [29] "e.3.Jan" "e.3.Dec"
paste_combine(x, y, sep = "_")
#>  [1] "a_1" "a_2" "a_3" "b_1" "b_2" "b_3" "c_1" "c_2" "c_3" "d_1" "d_2" "d_3"
#> [13] "e_1" "e_2" "e_3"
paste_combine(x, y, collate = FALSE)
#>  [1] "a1" "b1" "c1" "d1" "e1" "a2" "b2" "c2" "d2" "e2" "a3" "b3" "c3" "d3" "e3"
collapse0(list(1:3, letters[1:3]), 5:7, letters[5:7])
#> [1] "123abc567efg"
collapse0(1:3, letters[5:7], sep = "_")
#> [1] "1_2_3_e_f_g"
```
