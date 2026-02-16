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
#>  [1] "Lorem ipsum dolor sit amet, sed lorem, mauris,"    
#>  [2] "lacinia ac lobortis ultrices conubia. Sed at,"     
#>  [3] "risus nec habitant, eu ac euismod congue. Id donec"
#>  [4] "lorem vivamus id, nec. Euismod, phasellus sit est" 
#>  [5] "mauris, lorem ipsum porttitor litora gravida."     
#>  [6] "Dolor ac duis sit magna, iaculis tortor. Nam,"     
#>  [7] "pellentesque ridiculus efficitur sed placerat"     
#>  [8] "praesent cursus. Neque bibendum ex nullam in"      
#>  [9] "quisque elementum in efficitur? Sed, vitae"        
#> [10] "sagittis a conubia. Penatibus nullam pharetra,"    
#> [11] "donec, sociosqu mus. Vitae pharetra velit aenean," 
#> [12] "tellus quis massa ultrices et ut tellus ut aptent."
#> [13] "Nisl nulla, lectus torquent lectus vitae varius"   
#> [14] "est. Vitae, nisl quisque mauris interdum nec."     
```
