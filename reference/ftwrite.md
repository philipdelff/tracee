# Write flextable objects to one or multiple file formats

Writes `flextable` objects to various file formats with optional
stamping (caption with script name, timestamp, and model information).
Can write single flextables or lists of flextables, with automatic file
naming and directory structure organization.

## Usage

``` r
ftwrite(
  ft,
  file,
  formats,
  save = TRUE,
  show = !save,
  quiet = FALSE,
  script,
  time,
  model,
  stamp.ft.style,
  path.format = NULL,
  subdir = NULL,
  use.names = TRUE,
  stamp.format = NULL,
  stamp.funs = NULL,
  fun.path = NULL
)

writeFlextab(ft, file, script, formats, save, quiet = FALSE, ...)
```

## Arguments

- ft:

  A `flextable` object or a named list of `flextable` objects. If a list
  is provided, each element will be written to a separate file with
  names derived from the list element names.

- file:

  Character string specifying the base file name to save to (without
  extension). The actual extension(s) will be determined by the
  `formats` argument. If `ft` is a list and elements are named, those
  names will be appended to this base file name.

- formats:

  Character vector specifying one or more output formats. Supported
  formats are: "png", "docx", "pptx", "html", "rds". Default is `NULL`,
  which will infer the format from the `file` extension.

- save:

  Logical. Should the table be saved to file? Default is `TRUE`. Set to
  `FALSE` to only return/show the flextable without saving. Hint: if you
  use a flag variable (like `exportFlag`), you can use
  `save = exportFlag` to control output generation.

- show:

  Logical. Should the flextable be printed/displayed? Default is the
  opposite of `save` (i.e., `!save`). Combining `save = TRUE` and
  `show = TRUE` in knitr documents can be useful to display the table in
  the rendered document while also saving high-quality files.

- quiet:

  Logical. If `TRUE`, suppresses messages about files being written.
  Default is `FALSE`.

- script:

  Character string or path to the script file. If provided, will be
  included in the table stamp/caption. This helps document which script
  generated the output.

- time:

  Character string or POSIXct timestamp. The default behavior is to
  include a timestamp if `script` is provided. You can pass any string
  as `time` for a custom format, or use `time = ""` to omit the
  timestamp entirely.

- model:

  Character string or model object specifying the model name/identifier.
  Used for stamping and for organizing output directories via
  `path.format`.

- stamp.ft.style:

  A list of formatting options for the stamp, passed to
  [`ftstamp`](https://philipdelff.github.io/tracee/reference/ftstamp.md).
  Valid elements: `size`, `align`, `space`, `bg`, `color`.

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
  Used in conjunction with `path.format`.

- use.names:

  Logical. If `length(ft) > 1`, should `names(ft)` be used in the output
  file names? Default is `TRUE`. If `FALSE`, numeric indices (1, 2, 3,
  ...) are used instead. Only used if `save = TRUE` and `ft` is a list.

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

- fun.path:

  Deprecated. Use `path.format`.

- ...:

  Additional arguments passed to
  [`ftstamp`](https://philipdelff.github.io/tracee/reference/ftstamp.md)
  for stamp customization.

## Value

Invisibly returns the result from
[`ftwriteOne`](https://philipdelff.github.io/tracee/reference/ftwriteOne.md)
(for lists, returns results for all written tables). If `show = TRUE`,
also prints the flextable(s) to the current device.

## Details

The function handles both single flextable objects and lists of
flextables:

- For single flextables: writes to file(s) with name specified by `file`

- For lists of flextables: writes each element to a separate file, with
  names constructed from `file` and the list element names

When `ft` is a list:

- Named list elements use their names in the output file names

- Unnamed elements are automatically numbered with zero-padded indices

Multiple formats can be written simultaneously by specifying a vector of
formats. Each format will be saved as a separate file with the
appropriate extension.

## Functions

- `writeFlextab()`: Deprecated function name. Use ftwrite.

## File Formats

The following formats are supported via the `flextable` package save
functions:

- `"png"`: Image format via
  [`save_as_image`](https://davidgohel.github.io/flextable/reference/save_as_image.html)

- `"docx"`: Microsoft Word via
  [`save_as_docx`](https://davidgohel.github.io/flextable/reference/save_as_docx.html)

- `"pptx"`: Microsoft PowerPoint via
  [`save_as_pptx`](https://davidgohel.github.io/flextable/reference/save_as_pptx.html)

- `"html"`: HTML format via
  [`save_as_html`](https://davidgohel.github.io/flextable/reference/save_as_html.html)

- `"rds"`: R data format (saves the flextable object itself)

## See also

[`ftwriteOne`](https://philipdelff.github.io/tracee/reference/ftwriteOne.md)
for writing a single flextable,
[`ftstamp`](https://philipdelff.github.io/tracee/reference/ftstamp.md)
for adding stamps to flextables,
[`writer`](https://philipdelff.github.io/tracee/reference/writer.md) for
a unified interface to write various object types,
[`pathFormat`](https://philipdelff.github.io/tracee/reference/pathFormat.md)
for organizing output directories,
[`save_as_image`](https://davidgohel.github.io/flextable/reference/save_as_image.html),
[`save_as_docx`](https://davidgohel.github.io/flextable/reference/save_as_docx.html),
[`save_as_pptx`](https://davidgohel.github.io/flextable/reference/save_as_pptx.html),
[`save_as_html`](https://davidgohel.github.io/flextable/reference/save_as_html.html)

## Examples

``` r
if (FALSE) { # \dontrun{
library(flextable)

# Create a simple flextable
ft1 <- flextable(head(mtcars))

# Write to PNG (default format)
ftwrite(ft1, file = "mytable")

# Write to multiple formats
ftwrite(ft1, file = "mytable", formats = c("png", "docx", "html"))

# Write with stamping
ftwrite(ft1, file = "mytable",
        script = "analysis.R",
        model = "run001")

# Write a list of tables
ft_list <- list(
  summary = flextable(summary(mtcars)),
  head = flextable(head(mtcars)),
  tail = flextable(tail(mtcars))
)
ftwrite(ft_list, file = "tables", formats = "docx")
# Creates: tables_summary.docx, tables_head.docx, tables_tail.docx

# Use with path structure for model organization
ftwrite(ft1, file = "summary",
        model = "run001",
        path.format = "%model/%name_%model")
# Writes to: run001/summary_run001.png
} # }
```
