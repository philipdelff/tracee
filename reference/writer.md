# Write various R objects to files with flexible formatting

A unified interface for
[`ggwrite`](https://philipdelff.github.io/tracee/reference/ggwrite.md),
[`ftwrite`](https://philipdelff.github.io/tracee/reference/ftwrite.md),
and
[`NMwriteData`](https://nmautoverse.github.io/NMdata/reference/NMwriteData.html)
that automatically detects object type and applies the appropriate
writing function with consistent file naming and stamping conventions.

## Usage

``` r
writer(
  x,
  file,
  formats.ft,
  formats.gg,
  formats.data,
  path.format = NULL,
  subdir = NULL,
  stamp.format = NULL,
  stamp.funs = NULL,
  stamp.gg.style = NULL,
  stamp.ft.style = NULL,
  fun.path = NULL,
  ...
)
```

## Arguments

- x:

  Object to write. Can be a `ggplot`, `flextable`, `data.frame`, or list
  of such objects. Lists can be arbitrarily nested and will be processed
  according to object type.

- file:

  Character string specifying the base file name (without extension).
  The actual extension will be determined by the `formats.*` arguments.

- formats.ft:

  Character vector of file formats for flextable objects. Default is
  `"png"`. Supported formats include: "png", "docx", "pptx", "html". See
  [`ftwrite`](https://philipdelff.github.io/tracee/reference/ftwrite.md)
  for details.

- formats.gg:

  Character vector of file formats for ggplot objects. Default is
  `"png"`. Supported formats: "png", "pdf". See
  [`ggwrite`](https://philipdelff.github.io/tracee/reference/ggwrite.md)
  for details.

- formats.data:

  Character vector of file formats for data frame objects. Default is
  `"rds"`. This is passed to
  [`NMwriteData`](https://nmautoverse.github.io/NMdata/reference/NMwriteData.html)
  as the `format.write` argument. Common formats include: "rds", "csv",
  "fst".

- path.format:

  Character string or function defining the output path. A template
  using `%model`, `%script`, and `%name` placeholders, or a function as
  returned by
  [`pathFormat`](https://philipdelff.github.io/tracee/reference/pathFormat.md).
  Passed to
  [`pathFormat`](https://philipdelff.github.io/tracee/reference/pathFormat.md).
  Default (`NULL`) uses `file` as is.

- subdir:

  Character string specifying a subdirectory within the path structure.
  This is passed to the path function generated from `path.format`.

- stamp.format:

  A template for the stamp text using the placeholders `%script`,
  `%time`, `%model`, and `%file`, with `"\n"` for line breaks. Default
  is `"%file\n%script\n%model\n%time"`. See
  [`createStamp`](https://philipdelff.github.io/tracee/reference/createStamp.md).

- stamp.funs:

  Optional named list of functions (elements `script`, `time`, `model`,
  `file`) used to preprocess the values before they are inserted into
  `stamp.format`. See
  [`createStamp`](https://philipdelff.github.io/tracee/reference/createStamp.md).

- stamp.gg.style:

  A list of formatting options for the stamp text, passed to
  [`ggstamp`](https://philipdelff.github.io/tracee/reference/ggstamp.md).
  For ggplot objects, elements accepted by
  [`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html)
  (e.g. `size`, `colour`, `hjust`).

- stamp.ft.style:

  A list of formatting options for the stamp, passed to
  [`ftstamp`](https://philipdelff.github.io/tracee/reference/ftstamp.md).
  Valid elements: `size`, `align`, `space`, `bg`, `color`.

- fun.path:

  Deprecated. Use `path.format`.

- ...:

  Additional arguments passed to the specific writer functions
  ([`ggwrite`](https://philipdelff.github.io/tracee/reference/ggwrite.md),
  [`ftwrite`](https://philipdelff.github.io/tracee/reference/ftwrite.md),
  or
  [`NMwriteData`](https://nmautoverse.github.io/NMdata/reference/NMwriteData.html)).
  Common arguments include:

  - `script`: Path to the script file (for stamping)

  - `time`: Time stamp (for stamping)

  - `model`: Model identifier (for stamping and path construction)

  - `save`: Logical, whether to save the output (default `TRUE`)

  - `show`: Logical, whether to show/print the output (default `!save`)

  - `canvas`: Canvas size for plots (see
    [`canvasSize`](https://philipdelff.github.io/tracee/reference/canvasSize.md))

  - `quiet`: Logical, suppress messages (default `FALSE`)

## Value

Invisibly returns the result from the called writer function. The
specific return value depends on which writer function was dispatched
to.

## Details

The function dispatches to appropriate writer functions based on object
class:

- `flextable` objects →
  [`ftwrite`](https://philipdelff.github.io/tracee/reference/ftwrite.md)

- `data.frame` objects →
  [`datwrite`](https://philipdelff.github.io/tracee/reference/datwrite.md)
  (which wraps
  [`NMwriteData`](https://nmautoverse.github.io/NMdata/reference/NMwriteData.html))

- `ggplot`/`gtable` objects or lists →
  [`ggwrite`](https://philipdelff.github.io/tracee/reference/ggwrite.md)

For lists of objects, each element is written separately with
appropriate naming. The function intelligently handles nested lists by
flattening them and constructing meaningful file names from the list
structure.

File paths are constructed using `path.format`, which can organize
outputs by model, file name, or custom patterns. This is particularly
useful in modeling workflows where outputs need to be organized by model
number or name.

## Object Type Detection

The function determines object type in the following order:

1.  Check if `flextable`

2.  Check if `data.frame`

3.  Check if `ggplot` or `gtable` (using
    [`is.gg`](https://philipdelff.github.io/tracee/reference/is.gg.md))

4.  Check if `list` (and not a plot object)

5.  If none of the above, throw an error

## Trace Attributes

If the object `x` has traceit attributes (added via
[`traceit`](https://philipdelff.github.io/tracee/reference/traceit.md)),
those attributes will be automatically extracted and used by the writer
function, unless explicitly overridden by arguments passed to `writer`.

## See also

[`ggwrite`](https://philipdelff.github.io/tracee/reference/ggwrite.md)
for writing ggplot objects,
[`ftwrite`](https://philipdelff.github.io/tracee/reference/ftwrite.md)
for writing flextable objects,
[`datwrite`](https://philipdelff.github.io/tracee/reference/datwrite.md)
for writing data frames,
[`lwrite`](https://philipdelff.github.io/tracee/reference/lwrite.md) for
writing lists with subdirectory organization,
[`pathFormat`](https://philipdelff.github.io/tracee/reference/pathFormat.md)
for path format options,
[`traceit`](https://philipdelff.github.io/tracee/reference/traceit.md)
for adding traceit attributes to objects

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
library(flextable)

# Write a ggplot
p1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
writer(p1, file = "myplot", formats.gg = c("png", "pdf"))

# Write a flextable
ft1 <- flextable(head(mtcars))
writer(ft1, file = "mytable", formats.ft = c("png", "docx"))

# Write a data frame
writer(mtcars, file = "mydata", formats.data = "csv")

# Use with model organization
writer(p1, file = "gof", model = "run001", path.format = "%model/%name_%model")
# Writes to: run001/gof_run001.png

# Write with stamping
writer(p1, file = "myplot",
       script = "analysis.R",
       time = Sys.time(),
       model = "run001")
} # }
```
