# Fizz Buzz

For when someone asked you to do something you've done before, you can
argue that the quickest way to do it is to just take the work someone
else did and utilize that. No reason to reinvent the wheel.

## Usage

``` r
fizzbuzz(n, show_numbers = TRUE)

fizzbuzz_lazy(n)

.fizzbuzz_vector
```

## Format

An object of class `character` of length 1000000.

## Arguments

- n:

  The number of numbers

- show_numbers:

  If `TRUE` shows no

## Value

A `character` vector of `1, 2, Fizz, 3, Buzz`, etc

## Details

Multiples of `3` are shown as `"Fizz"`; multiples of `5` as `"Buzz"`;
multiple of both (i.e., `15`) are `"FizzBuzz"`. `fizzbuzz_lazy()`
subsets the `.fizzbuzz_vector` object, which is a solution with default
parameters up to `1e6`

## Examples

``` r
fizzbuzz(15)
#>  [1] "1"        "2"        "Fizz"     "4"        "Buzz"     "Fizz"    
#>  [7] "7"        "8"        "Fizz"     "Buzz"     "11"       "Fizz"    
#> [13] "13"       "14"       "FizzBuzz"
fizzbuzz(30, show_numbers = FALSE)
#>  [1] ""         ""         "Fizz"     ""         "Buzz"     "Fizz"    
#>  [7] ""         ""         "Fizz"     "Buzz"     ""         "Fizz"    
#> [13] ""         ""         "FizzBuzz" ""         ""         "Fizz"    
#> [19] ""         "Buzz"     "Fizz"     ""         ""         "Fizz"    
#> [25] "Buzz"     ""         "Fizz"     ""         ""         "FizzBuzz"
cat(fizzbuzz(30), sep = "\n")
#> 1
#> 2
#> Fizz
#> 4
#> Buzz
#> Fizz
#> 7
#> 8
#> Fizz
#> Buzz
#> 11
#> Fizz
#> 13
#> 14
#> FizzBuzz
#> 16
#> 17
#> Fizz
#> 19
#> Buzz
#> Fizz
#> 22
#> 23
#> Fizz
#> Buzz
#> 26
#> Fizz
#> 28
#> 29
#> FizzBuzz

# \donttest{
# show them how fast your solution is:
if (package_available("bench")) {
  bench::mark(fizzbuzz(1e5), fizzbuzz_lazy(1e5))
}
#> # A tibble: 2 × 13
#>   expression      min median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time
#>   <bch:expr>  <bch:t> <bch:>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm>
#> 1 fizzbuzz(1…  33.6ms   34ms      28.4    5.19MB     4.74    12     2      422ms
#> 2 fizzbuzz_l… 564.8µs  581µs    1697.    38.21MB    66.9    507    20      299ms
#> # ℹ 4 more variables: result <list>, memory <list>, time <list>, gc <list>
# }
```
