# Write lists of outputs with flexible organization and naming

Writes nested lists containing plots, tables, and data frames to files
with intelligent handling of list structure, automatic naming, and
optional subdirectory organization. This is particularly useful for
organizing multiple related outputs from a single analysis or model run.

## Usage

``` r
lwrite(
  list,
  model,
  dir = ".",
  path.format,
  lists.as.subdirs = FALSE,
  name.all.pdf = "main_all.pdf",
  structure,
  ...
)
```

## Arguments

- list:

  A list (potentially nested) containing output objects: `ggplot` plots,
  `flextable` tables, and/or `data.frame`s. The list can be arbitrarily
  nested, and names at each level will be used to construct output file
  names.

- model:

  Character string or model object specifying the model name/identifier.
  Used for organizing output directories and stamping. If a list (model
  object) is provided, the `$label` element is extracted.

- dir:

  Character string specifying the base output directory. Default is
  `"."` (current directory).

- path.format:

  Character string or function defining the output path format. Passed
  to
  [`pathFormat`](https://philipdelff.github.io/tracee/reference/pathFormat.md)
  to create a path-generating function. Examples:
  `"%model/%name_%model"` (default), `"%name_%model"`,
  `"%script/%name_%model"`. See
  [`pathFormat`](https://philipdelff.github.io/tracee/reference/pathFormat.md)
  for details.

- lists.as.subdirs:

  Logical. If `TRUE`, nested lists create subdirectories rather than
  being incorporated into file names. Default is `FALSE`.

  - `FALSE`: nested list names become part of file names (e.g.,
    `list1_item1.png`)

  - `TRUE`: nested lists create subdirectories (e.g., `list1/item1.png`)

- name.all.pdf:

  Character string specifying the filename to use when writing all plots
  to a single PDF file. Only used when the file name is empty and plots
  are being combined into one PDF. Default is `"main_all.pdf"`.

- structure:

  Deprecated. Use `path.format`.

- ...:

  Additional arguments passed to
  [`writer`](https://philipdelff.github.io/tracee/reference/writer.md)
  and subsequently to the specific writer functions
  ([`ggwrite`](https://philipdelff.github.io/tracee/reference/ggwrite.md),
  [`ftwrite`](https://philipdelff.github.io/tracee/reference/ftwrite.md),
  or
  [`datwrite`](https://philipdelff.github.io/tracee/reference/datwrite.md)).
  Common arguments include:

  - `script`: Path to script file for stamping

  - `time`: Timestamp for stamping

  - `formats.gg`: File formats for plots (default "png")

  - `formats.ft`: File formats for tables (default "png")

  - `formats.data`: File formats for data frames (default "rds")

  - `canvas`: Canvas size for plots

  - `quiet`: Suppress messages

  - `stamp.format`, `stamp.funs`: Stamp text format, see
    [`createStamp`](https://philipdelff.github.io/tracee/reference/createStamp.md)

## Value

Invisibly returns a list of results from the
[`writer`](https://philipdelff.github.io/tracee/reference/writer.md)
calls. The structure mirrors the input list structure.

## Details

The function processes lists recursively with the following behavior:

1.  Separates plot objects from non-plot objects

2.  Combines plots into a single multi-page or multi-file output (see
    [`ggwrite`](https://philipdelff.github.io/tracee/reference/ggwrite.md))

3.  Writes each non-plot object individually

4.  Handles nested lists according to `lists.as.subdirs`

## List Structure Handling

The function intelligently flattens nested lists while preserving
meaningful names:

- Named list elements use their names in output file names

- Plots in the same list are combined (behavior depends on
  [`ggwrite`](https://philipdelff.github.io/tracee/reference/ggwrite.md)
  settings)

- Non-plot objects (tables, data frames) are written individually

- Nesting deeper than 2 levels will cause an error

## File Naming

Output file names are constructed from:

- The name at each level of the list hierarchy

- The `model` identifier (if provided and if `path.format` uses it)

- Automatic numbering for unnamed elements

For example, with `lists.as.subdirs = FALSE`:


    list(
      section1 = list(
        plot1 = p1,
        table1 = ft1
      )
    )

produces: `section1_plot1.png`, `section1_table1.png`

With `lists.as.subdirs = TRUE`, the same structure produces:
`section1/plot1.png`, `section1/table1.png`

## Plot Handling

All `ggplot` objects within the same list level are collected and
written together using
[`ggwrite`](https://philipdelff.github.io/tracee/reference/ggwrite.md).
This allows creating multi-page PDFs or sequentially numbered PNG files
from a list of plots.

## See also

[`writer`](https://philipdelff.github.io/tracee/reference/writer.md) for
the underlying writing function,
[`ggwrite`](https://philipdelff.github.io/tracee/reference/ggwrite.md)
for plot-specific options,
[`ftwrite`](https://philipdelff.github.io/tracee/reference/ftwrite.md)
for table-specific options,
[`datwrite`](https://philipdelff.github.io/tracee/reference/datwrite.md)
for data frame writing,
[`pathFormat`](https://philipdelff.github.io/tracee/reference/pathFormat.md)
for path format options,
[`is.listnotplot`](https://philipdelff.github.io/tracee/reference/is.listnotplot.md)
for list detection logic,
[`unwrapOutputs`](https://philipdelff.github.io/tracee/reference/unwrapOutputs.md)
for an alternative list flattening approach

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
library(flextable)

# Create some outputs
p1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
p2 <- ggplot(mtcars, aes(hp, mpg)) + geom_point()
ft1 <- flextable(head(mtcars))
df1 <- mtcars

# Simple list
outputs <- list(
  scatter_wt = p1,
  scatter_hp = p2,
  summary_table = ft1,
  data = df1
)

lwrite(outputs, model = "run001", dir = "outputs",script="myscript")
# Creates in outputs/run001/:
#   scatter_wt_run001.png
#   scatter_hp_run001.png
#   summary_table_run001.png
#   data_run001.rds

# Nested list with subdirectories
nested_outputs <- list(
  diagnostics = list(
    gof1 = p1,
    gof2 = p2
  ),
  tables = list(
    summary = ft1
  )
)

lwrite(nested_outputs,
       model = "run001",
       dir = "outputs",
       lists.as.subdirs = TRUE)
# Creates:
#   outputs/run001/diagnostics/gof1_run001.png
#   outputs/run001/diagnostics/gof2_run001.png
#   outputs/run001/tables/summary_run001.png

# With stamping
lwrite(outputs,
       model = "run001",
       dir = "outputs",
       script = "analysis.R",
       time = Sys.time())
} # }
```
