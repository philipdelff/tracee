# Export ggplot plots (and related objects) to files with stamping support

Writes `ggplot` plots, `gtable` objects, or lists thereof to PNG or PDF
files with optional stamping (caption with script name, timestamp, and
model information). Supports flexible canvas sizing, multiple output
formats, and both single-file and multi-file output for lists of plots.

## Usage

``` r
ggwrite(
  plot,
  file,
  canvas = NULL,
  formats,
  onefile = FALSE,
  res = 200,
  paper = "special",
  save = TRUE,
  show = !save,
  use.names = FALSE,
  subdir = NULL,
  script,
  time,
  model,
  name.all.pdf,
  quiet = FALSE,
  path.format = NULL,
  method.png = c("ragg", "png"),
  useNames,
  fun.path = NULL
)
```

## Arguments

- plot:

  A plot object or a list of plot objects. Normally generated with
  `ggplot` or `qplot`. Can also be `gtable` objects from
  [`grid.arrange`](https://rdrr.io/pkg/gridExtra/man/arrangeGrob.html)
  or
  [`arrangeGrob`](https://rdrr.io/pkg/gridExtra/man/arrangeGrob.html).
  Support for `gtable` is experimental.

- file:

  Character string specifying the output file path. Must end in `.png`
  or `.pdf`. If missing and `save = TRUE`, an error is raised. If
  missing and `save = FALSE`, the plot is shown on screen instead. For
  lists of plots, see `onefile` for naming behavior.

- canvas:

  Either a list with `height` and `width` elements (in inches), or a
  character string shortname of a predefined canvas size. Default is
  `"standard"`. See
  [`canvasSize`](https://philipdelff.github.io/tracee/reference/canvasSize.md)
  for available sizes and custom size specification. Multiple canvas
  sizes can be specified as a named list to generate outputs at
  different sizes.

- formats:

  Character vector specifying file format(s) to write. Must be a subset
  of `c("png", "pdf")`. Default is to use the format matching the `file`
  extension. Multiple formats can be specified to write the same plot in
  different formats.

- onefile:

  Logical. Only applicable if `plot` is a list. If `TRUE`, all plots are
  written to a single multi-page PDF (`file` must end in `.pdf`). If
  `FALSE`, numbered files are created - one per list element. Default is
  `FALSE`.

- res:

  Numeric. Resolution in pixels per inch for PNG output. Default is 200.
  Passed to [`png`](https://rdrr.io/r/grDevices/png.html).

- paper:

  Character string specifying paper type. Only used with PDF device.
  Default is `"special"`. See
  [`pdf`](https://rdrr.io/r/grDevices/pdf.html) for options.

- save:

  Logical. Should the plot be saved to file? Default is `TRUE`. Set to
  `FALSE` to only display the plot without saving. Useful when
  controlled by a flag variable (e.g., `save = writeOutputs`).

- show:

  Logical. Should the plot be displayed on screen? Default is the
  opposite of `save` (i.e., `!save`). Setting both `save = TRUE` and
  `show = TRUE` in knitr documents can display the plot in the rendered
  document while also saving high-quality files.

- use.names:

  Logical. If `length(plot) > 1`, should `names(plot)` be used in the
  output file names? Default is `FALSE`, which uses numeric indices (1,
  2, 3, ...). Only used if `save = TRUE` and `plot` is a list.

- subdir:

  Character string specifying a subdirectory within the path structure.
  This is passed to the path function generated from `path.format`.
  Default is `NULL`.

- script:

  Character string or path to the script file. If provided, a stamp
  (caption) will be added to the plot showing the script name,
  timestamp, and optionally the model. Requires `ggplot2` \>= 2.2.1.

- time:

  Character string or POSIXct timestamp. By default, a timestamp is
  included if `script` is provided. You can pass a custom time string or
  use `time = ""` to omit the timestamp.

- model:

  Character string or model object specifying the model name/identifier.
  Used for stamping and for organizing output directories via
  `fun.path`.

- name.all.pdf:

  Character string specifying the filename to use when writing all plots
  to a single PDF file (when `onefile = TRUE`). Only used when
  `file = ""`. Default is `"all.pdf"`.

- quiet:

  Logical. If `TRUE`, suppresses messages about files being written.
  Default is `FALSE`.

- path.format:

  Character string or function defining the output path. A template
  using `%model`, `%script`, and `%name` placeholders, or a function as
  returned by
  [`pathFormat`](https://philipdelff.github.io/tracee/reference/pathFormat.md).
  Passed to
  [`pathFormat`](https://philipdelff.github.io/tracee/reference/pathFormat.md).
  Default (`NULL`) uses `file` as is.

- method.png:

  Character string, one of `"png"` or `"ragg"`, controlling which
  graphics device is used for PNG output. `"png"` uses the base
  [`png`](https://rdrr.io/r/grDevices/png.html) device; `"ragg"` uses
  [`agg_png`](https://ragg.r-lib.org/reference/agg_png.html), which
  produces output that renders more consistently across platforms
  (recommended for reproducible/snapshot-tested figures). Default is
  `"png"`.

- useNames:

  Deprecated. Use `use.names` instead.

- fun.path:

  Deprecated. Use `path.format`.

## Value

Returns `NULL` invisibly. Files are written as a side effect. If
`show = TRUE`, plots are also displayed.

## Details

The function handles both single plot objects and lists of plots with
flexible output options:

**Single plots:**

- Written to `file` with specified format(s)

- Can generate multiple formats simultaneously

- Can generate multiple canvas sizes simultaneously

**Lists of plots:**

- If `onefile = TRUE`: creates a single multi-page PDF

- If `onefile = FALSE`: creates separate files (numbered or named)

- Automatic naming based on list element names or indices

## Canvas Sizes

Predefined canvas sizes (see
[`canvasSize`](https://philipdelff.github.io/tracee/reference/canvasSize.md)):

- `"standard"`: 7.2 × 5.4 inches

- `"wide"`: 9.6 × 5.4 inches

- `"wide-screen"`: 18.6 × 9 inches

- `"square"`: 5.4 × 5.4 inches

- `"A4"`: 5.4 × 7.2 inches

- `"letter"`: 6.5 × 9 inches

Custom sizes can be specified as `list(width = w, height = h)` where `w`
and `h` are in inches.

## Stamping

When `script` is provided, a caption is added to the plot using
[`ggstamp`](https://philipdelff.github.io/tracee/reference/ggstamp.md).
The stamp typically includes:

- Script file name

- Timestamp (unless `time = ""`)

- Model identifier (if `model` is provided)

The stamp is added as a ggplot2 caption and will appear at the bottom of
the plot.

## Trace Attributes

If `plot` has traceit attributes (added via
[`traceit`](https://philipdelff.github.io/tracee/reference/traceit.md)),
those attributes will be automatically extracted and used, unless
explicitly overridden by arguments passed to `ggwrite`.

## See also

[`ggstamp`](https://philipdelff.github.io/tracee/reference/ggstamp.md)
for adding stamps to plots,
[`canvasSize`](https://philipdelff.github.io/tracee/reference/canvasSize.md)
for canvas size options,
[`writer`](https://philipdelff.github.io/tracee/reference/writer.md) for
a unified interface to write various object types,
[`pathFormat`](https://philipdelff.github.io/tracee/reference/pathFormat.md)
for organizing output directories,
[`traceit`](https://philipdelff.github.io/tracee/reference/traceit.md)
for adding traceit attributes,
[`png`](https://rdrr.io/r/grDevices/png.html),
[`pdf`](https://rdrr.io/r/grDevices/pdf.html) for graphics devices

Other Plotting:
[`canvasSize()`](https://philipdelff.github.io/tracee/reference/canvasSize.md),
[`ggstamp()`](https://philipdelff.github.io/tracee/reference/ggstamp.md),
[`ggwater()`](https://philipdelff.github.io/tracee/reference/ggwater.md)

## Examples

``` r
library(ggplot2)
writeOutput <- FALSE
data(ChickWeight)
p1 <- ggplot(ChickWeight, aes(Time, weight, group = Chick, colour = factor(Diet))) +
  geom_line()

# View plot on screen
ggwrite(p1)


# Save plot with stamping
script <- "analysis.R"
ggwrite(p1, script = script, canvas = "wide",
        file = "myplot1.png", save = writeOutput)


# Save in multiple formats
ggwrite(p1, file = "myplot1.png",
        formats = c("png", "pdf"),
        save = writeOutput)


# Multiple canvas sizes
ggwrite(p1, file = "myplot1.png",
        canvas = list(small = "standard", large = "wide"),
        save = writeOutput)

# Creates: myplot1_small.png, myplot1_large.png

# List of plots to separate files
p2 <- ggplot(ChickWeight, aes(Time, weight)) + geom_point()
plots <- list(lines = p1, points = p2)
ggwrite(plots, file = "chickweight.png",
        use.names = TRUE, save = writeOutput)


# Creates: chickweight_lines.png, chickweight_points.png

# List of plots to single PDF
ggwrite(plots, file = "chickweight.pdf",
        onefile = TRUE, save = writeOutput)


# Creates: chickweight.pdf (2 pages)

# Organize output using a %script-based directory structure
ggwrite(p1, file = "chickweight_lines.png",
        script = script, path.format = "%script/%name",
        save = writeOutput)

# Creates: analysis/chickweight_lines.png
```
