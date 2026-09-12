# Time reports

**\[experimental\]** This function can be used to evaluate an expression
line-by-line to capture outputs, errors, messages, and evaluation time.

## Usage

``` r
simpleTimeReport(title = NULL, expr, envir = parent.frame())
```

## Arguments

- title:

  The title to be printed

- expr:

  The expression to run

- envir:

  The environment from which to evaluate the `expr`

## Value

A `reported_results`/`list` object containing results, outputs,
messages, warnings, and errors

## Details

Evaluate code and report on the time difference

## Examples

``` r
simpleTimeReport("example", {
  print("1")
  Sys.sleep(1)
  warning("this is a warning")
  for (i in 1:5) {
    Sys.sleep(0.5)
  }
  sample(1e6, 1e6, TRUE)
})
#> example
#> --------------------------------------------------------------------------------
#> print("1")                        
#> Sys.sleep(1)                      [1.00 s]
#> warning("this is a warning")      
#> for (i in 1:5) { Sys.sleep(0.5)}  [2.50 s]
#> sample(1e+06, 1e+06, TRUE)        
#> --------------------------------------------------------------------------------
#> Finished [3.53 s]
#> 
#> Warnings
#> --------------------------------------------------------------------------------
#> 3 : warning("this is a warning")
#> Warning: this is a warning
#> 
#> Outputs
#> --------------------------------------------------------------------------------
#> 1 : print("1")
#> [1] "1"
```
