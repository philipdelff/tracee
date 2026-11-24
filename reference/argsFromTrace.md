# Extract arguments from a traceit object

This function retrieves stored arguments from a traceit object's
attributes and returns those that match the formal arguments of a given
function, excluding arguments that were already supplied in the current
call.

## Usage

``` r
argsFromTrace(fun, x, attr = "args")
```

## Arguments

- fun:

  A function whose formal arguments will be matched against the stored
  arguments in the traceit object

- x:

  An object to extract arguments from. Must inherit from "traceit"
  class, otherwise an empty list is returned

- attr:

  Character string specifying the name of the attribute containing the
  stored arguments. Default is "args"

## Value

A named list of arguments that:

- Match formal arguments of `fun` (excluding "x")

- Were stored in the traceit object's attributes

- Were NOT already supplied in the current function call

Returns an empty list if `x` does not inherit from "traceit" or if the
specified attribute is not a list.

## Details

This function is used internally to retrieve default arguments that were
previously stored in a traceit object (via
[`traceit()`](https://nmautoverse.github.io/tracee/reference/traceit.md)
functions) and apply them to subsequent function calls. It ensures that
explicitly provided arguments take precedence over stored defaults.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming a traceit object with stored arguments
p <- traceit(ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point(),
             canvas = "wide", formats = "png")
# argsFromTrace would extract canvas and formats if not already specified
args <- argsFromTrace(ggwrite, p)
} # }
```
