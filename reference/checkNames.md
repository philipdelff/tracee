# Check if list element names are allowed

Internal function to validate that all names in a list are within a set
of allowed element names. Throws an error if any disallowed names are
found.

## Usage

``` r
checkNames(x, elems.allowed = NULL, name.x = "x")
```

## Arguments

- x:

  A named list to check.

- elems.allowed:

  Character vector of allowed element names. If NULL, all names are
  allowed and the function returns x unchanged.

- name.x:

  Character string used in error messages to identify the object being
  checked. Default is "x".

## Value

Returns x unchanged if all names are allowed.

## Details

This function is used internally to validate argument lists before
passing them to other functions. It helps catch typos or invalid
arguments early.
