# Check if an object is a list but not a plot, data frame, or flextable

This function tests whether an object is a list that is NOT a
ggplot/gtable, data frame, or flextable object. This is useful for
identifying nested list structures that contain multiple outputs.

## Usage

``` r
is.listnotplot(x)
```

## Arguments

- x:

  An object to test

## Value

Logical. TRUE if x is a list but not a ggplot, gtable, data.frame, or
flextable object; FALSE otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
p <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
is.listnotplot(p)  # FALSE (it's a ggplot)
is.listnotplot(mtcars)  # FALSE (it's a data.frame)
is.listnotplot(list(a = 1, b = 2))  # TRUE (it's a plain list)
} # }
```
