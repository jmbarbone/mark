# Import

Import a single function from a package

## Usage

``` r
import(pkg, fun, overwrite = FALSE)
```

## Arguments

- pkg:

  String, name of the package

- fun:

  String, fun name of the function

- overwrite:

  Logical, if TRUE and `fun` is also found in the current environment,
  will overwrite assignment

## Value

None, called for side effects

## Examples

``` r
# assigns `add` -- test with caution
import("magrittr", "add")
```
