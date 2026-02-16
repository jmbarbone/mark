# Evaluate a Named Chunk

Evaluate a named chunk from an Rmd file.

## Usage

``` r
eval_named_chunk(rmd_file, label_name)
```

## Arguments

- rmd_file:

  Absolute path to rmd file

- label_name:

  Name of label

## Value

The value from the evaluated code chunk

## Examples

```` r
temp_rmd <- tempfile(fileext = ".rmd")

text <- '
```{r not this label}
print("that is wrong")
```

```{r hello label}
text <- "hello, world"
print(text)
print(TRUE)
```

```{r another label}
warning("wrong label")
```
'
if (FALSE) { # \dontrun{
writeLines(text, con = temp_rmd)

eval_named_chunk(temp_rmd, "hello label")
# [1] "hello, world"
# [1] TRUE

file.remove(temp_rmd)
} # }
````
