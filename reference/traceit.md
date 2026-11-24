# Add tracing attributes to R objects for automated output writing

The `traceit` family of functions attaches metadata to R objects that
can be used later by writer functions to automatically determine output
settings. This allows you to "trace" or "tag" objects with information
about how they should be written, then write them later without having
to re-specify those settings.

## Usage

``` r
traceit(x, ...)

# S3 method for class 'ggplot'
traceit(x, ..., quite = TRUE)

# S3 method for class 'flextable'
traceit(x, ..., quite = TRUE)

# S3 method for class 'data.frame'
traceit(x, ..., quite = TRUE)

# S3 method for class 'traceit'
traceit(x, ..., quite = TRUE)
```

## Arguments

- x:

  An R object to attach trace attributes to. Can be a `ggplot`,
  `flextable`, or `data.frame` object.

- ...:

  Named arguments that will be stored as trace attributes. For ggplot
  objects, valid arguments match those in
  [`ggwrite`](https://nmautoverse.github.io/tracee/reference/ggwrite.md).
  For flextable objects, valid arguments match those in
  [`ftwrite`](https://nmautoverse.github.io/tracee/reference/ftwrite.md).
  For data.frame objects, valid arguments match those in
  [`NMwriteData`](https://nmautoverse.github.io/NMdata/reference/NMwriteData.html).

- quite:

  Logical. If `TRUE`, suppresses messages. Default is `TRUE`. Note: this
  parameter name appears to be a typo and should likely be "quiet".

## Value

Returns `x` invisibly with trace attributes attached. The object can be
used normally and the trace attributes will be preserved.

## Details

The `traceit` function is a generic that dispatches to specific methods
based on the class of `x`:

- `traceit.ggplot`: For ggplot objects

- `traceit.flextable`: For flextable objects

- `traceit.data.frame`: For data frames

- `traceit.traceit`: For objects that already have traceit attributes
  (allows updating existing trace information)

When an object is traced, two attributes are added:

- `args`: A list of arguments to be used when writing the object

- `class`: The class vector is prepended with "traceit" to indicate the
  object has been traced

Arguments passed to `traceit` are filtered to include only those that
are valid for the corresponding writer function. Invalid arguments are
silently ignored.

When calling `traceit` on an already-traced object, the new arguments
are merged with existing trace arguments using
[`modifyList`](https://rdrr.io/r/utils/modifyList.html), with new
arguments taking precedence.

## Extracting Trace Attributes

To extract trace attributes from an object, use
[`argsFromTrace`](https://nmautoverse.github.io/tracee/reference/argsFromTrace.md).

## See also

[`ggwrite`](https://nmautoverse.github.io/tracee/reference/ggwrite.md)
for writing ggplot objects,
[`ftwrite`](https://nmautoverse.github.io/tracee/reference/ftwrite.md)
for writing flextable objects,
[`NMwriteData`](https://nmautoverse.github.io/NMdata/reference/NMwriteData.html)
for writing data frames,
[`argsFromTrace`](https://nmautoverse.github.io/tracee/reference/argsFromTrace.md)
for extracting trace attributes,
[`writer`](https://nmautoverse.github.io/tracee/reference/writer.md) for
a unified writing interface

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
library(flextable)

# Create a plot and trace it with output settings
p1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
p1_traced <- traceit(p1, canvas = "wide", formats = c("png", "pdf"))

# The traced plot can be written later without re-specifying settings
# writer() will automatically use the traced arguments

# Trace a flextable
ft1 <- flextable(head(mtcars))
ft1_traced <- traceit(ft1, formats = "docx")

# Update trace attributes on an already-traced object
p1_traced <- traceit(p1_traced, canvas = "standard")  # overrides previous canvas

# Trace a data frame for NMwriteData
df1 <- data.frame(ID = 1:10, DV = rnorm(10))
df1_traced <- traceit(df1, file = "mydata.csv")
} # }
```
