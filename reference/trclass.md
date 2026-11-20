# Determine the class type of an R object for writer dispatch

Internal function that determines the appropriate class type of an
object for dispatching to the correct writer function. This function
classifies objects into one of several categories: "flextable",
"data.frame", "gg" (for ggplot/gtable objects), "list", or throws an
error for unknown types.

## Usage

``` r
trclass(x)
```

## Arguments

- x:

  An R object to classify. Can be a `flextable`, `data.frame`, `ggplot`,
  `gtable`, `list`, or object with "traceit" class.

## Value

A character string indicating the object class type:

- `"flextable"`: for flextable objects

- `"data.frame"`: for data frame objects

- `"gg"`: for ggplot or gtable objects

- `"list"`: for list objects (that are not plots)

## Details

The function checks object class in the following priority order:

1.  `flextable`: checked via `"flextable" %in% class(x)`

2.  `data.frame`: checked via `is.data.frame(x)`

3.  `gg`: checked via
    [`is.gg`](https://philipdelff.github.io/tracee/reference/is.gg.md)`(x)`
    (includes ggplot and gtable)

4.  `list`: checked via `is.list(x) && !is.gg(x)`

5.  If none match: throws an error

This function is primarily used internally by
[`writer`](https://philipdelff.github.io/tracee/reference/writer.md) and
[`traceit.traceit`](https://philipdelff.github.io/tracee/reference/traceit.md)
to determine which specific writer function to call (ggwrite, ftwrite,
or datwrite).

The function ignores the "traceit" class that may be prepended by
[`traceit`](https://philipdelff.github.io/tracee/reference/traceit.md),
allowing it to classify traced objects by their underlying type.

## See also

[`writer`](https://philipdelff.github.io/tracee/reference/writer.md) for
the main writer function that uses this classification,
[`traceit.traceit`](https://philipdelff.github.io/tracee/reference/traceit.md)
for traceit method that uses this,
[`is.gg`](https://philipdelff.github.io/tracee/reference/is.gg.md) for
plot object detection,
[`is.listnotplot`](https://philipdelff.github.io/tracee/reference/is.listnotplot.md)
for list detection logic

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
library(flextable)

# Classify different object types
trclass(mtcars)  # Returns: "data.frame"

p1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
trclass(p1)  # Returns: "gg"

ft1 <- flextable(head(mtcars))
trclass(ft1)  # Returns: "flextable"

trclass(list(p1, ft1))  # Returns: "list"

# Works with traced objects too
p1_traced <- traceit(p1, canvas = "wide")
trclass(p1_traced)  # Still returns: "gg"

# Unknown types throw an error
trclass(matrix(1:4, 2))  # Error: x is an unknown format
} # }
```
