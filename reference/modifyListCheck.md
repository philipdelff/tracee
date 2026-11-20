# Modify a list with validation of allowed elements

Internal function that combines two lists using
[`modifyList`](https://rdrr.io/r/utils/modifyList.html) while validating
that all element names are within a set of allowed names.

## Usage

``` r
modifyListCheck(x, y, elems.allowed)
```

## Arguments

- x:

  A named list with default values.

- y:

  A named list with values to override those in x. Can be NULL.

- elems.allowed:

  Character vector of allowed element names for both x and y.

## Value

If y is NULL, returns x (after validation). Otherwise returns the result
of modifyList(x, y) after validating both lists.

## Details

This function first validates that x contains only allowed element
names. If y is not NULL, it merges y into x using
[`modifyList`](https://rdrr.io/r/utils/modifyList.html), then validates
that the resulting list also contains only allowed names. This ensures
that both default arguments and user-provided overrides are valid.

## See also

[`checkNames`](https://philipdelff.github.io/tracee/reference/checkNames.md),
[`modifyList`](https://rdrr.io/r/utils/modifyList.html)
