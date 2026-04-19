# String Slice

Slice/split a string into multiple lines by the desired length of the
line.

## Usage

``` r
str_slice(x, n = 80L)

str_slice_by_word(x, n = 80L)
```

## Arguments

- x:

  A character vector

- n:

  Integer, the length of the line split

## Value

A `character` vector

## Examples

``` r
if (requireNamespace("stringi")) {
  x <- stringi::stri_rand_lipsum(1)
  str_slice(x)
  str_slice_by_word(x, n = 50L)
}
#> [1] "Lorem ipsum dolor sit amet, volutpat dapibus,"     
#> [2] "egestas sagittis felis in leo efficitur felis enim"
#> [3] "nisi! Eu libero, sed nullam metus mauris. Ipsum"   
#> [4] "quisque sed velit neque urna. Vitae elementum sed" 
#> [5] "ac sit a maximus dis ultrices, est, at id. Ac sed,"
#> [6] "lorem, mauris lacinia ac lobortis ultrices"        
#> [7] "conubia. Sed at. Risus nec habitant eu ac, euismod"
#> [8] "congue id donec. Lorem vivamus id nec euismod"     
#> [9] "phasellus sit est mauris lorem ipsum."             
```
