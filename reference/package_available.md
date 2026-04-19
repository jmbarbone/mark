# Check if package is available

A wrapped
[`base::requireNamespace()`](https://rdrr.io/r/base/ns-load.html)

## Usage

``` r
package_available(namespace)
```

## Arguments

- namespace:

  One or more packages to to require.

## Value

- [`require_namespace()`](https://jmbarbone.github.io/fuj/reference/require_namespace.html):
  None, called for side effects

- `package_available()`: Visibly, `TRUE` or `FALSE`
