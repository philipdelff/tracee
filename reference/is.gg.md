# Check if an object is a ggplot or gtable

This function tests whether an object is a ggplot2 plot object or a
gtable (grid table) object.

## Usage

``` r
is.gg(x)
```

## Arguments

- x:

  An object to test

## Value

Logical. TRUE if x inherits from "ggplot" or "gtable", FALSE otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
p <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
is.gg(p)  # TRUE
is.gg(mtcars)  # FALSE
} # }
```
