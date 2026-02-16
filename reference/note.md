# Append a note to an object

An alternative to the
[`base::comment()`](https://rdrr.io/r/base/comment.html).

## Usage

``` r
note(x) <- value

set_note(x, value)

note(x)
```

## Arguments

- x:

  An object

- value:

  The note to attach; if `NULL` will remove the note and the class
  `noted` from the object.

## Value

- `note<-`, `set_note()` will return `x` (with the `"note"` attribute
  assigned)

- `note()` will retrieve the `"note"` attribute

## Details

When the note is assigned to an object a new class will be added,
`note`, so that a `print` function can call an S3 method. The print for
this can be adjusted for it's width by using the option
`mark.note.width` which defaults to the option `width` when not set.

The type of object assigned to the note is not restricted, so user
beware of odd prints or additional features added to the notes fun.

When assigning a note (with `note<-`, and its alias `set_note()`) the
`noted` class is added to the object. This allows the `print.noted`
class to be dispatched and for the note to be printed every time the
object is called/printed and the next print method used. However, it
will not be called when not
[`interactive()`](https://rdrr.io/r/base/interactive.html)

## Examples

``` r
x <- c("x", "k", "c", "d")
comment(x) <- "This is just a comment"
comment(x)
#> [1] "This is just a comment"

# Comment is intentionally hidden
x
#> [1] "x" "k" "c" "d"
note(x) <- "Just some random letters"
note(x)
#> [1] "Just some random letters"

# Note is now present every time
x
#> [1] "x" "k" "c" "d"

# Assigning `NULL` will remove note (and class)
note(x) <- NULL
note(x) # NULL
#> NULL
x       # No more note
#> [1] "x" "k" "c" "d"
```
