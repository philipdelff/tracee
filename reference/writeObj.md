# Write a plot or list of plots to one or more files

Internal workhorse function called by
[`ggwrite`](https://nmautoverse.github.io/tracee/reference/ggwrite.md).
Resolves the combination of output formats and canvas sizes (via
[`ggwrite_names`](https://nmautoverse.github.io/tracee/reference/ggwrite_names.md)),
and dispatches to
[`write1`](https://nmautoverse.github.io/tracee/reference/write1.md) for
each individual plot/format/canvas combination. Handles both single plot
objects and lists of plots, including the `onefile` (multi-page PDF)
case.

## Usage

``` r
writeObj(
  plot,
  file,
  script,
  time,
  model = model,
  onefile,
  use.names = FALSE,
  formats,
  canvas,
  quiet = FALSE,
  fun.path,
  subdir,
  name.all.pdf = "all.pdf",
  method.png = c("ragg", "png"),
  ...
)
```

## Arguments

- plot:

  A plot object or a list of plot objects. See
  [`ggwrite`](https://nmautoverse.github.io/tracee/reference/ggwrite.md).

- file:

  Character string, base output file path, or `NULL` to only display the
  plot(s) on screen.

- script:

  Character string or path to the calling script, used for stamping. See
  [`ggstamp`](https://nmautoverse.github.io/tracee/reference/ggstamp.md).

- time:

  Character string or POSIXct timestamp used for stamping.

- model:

  Character string or model object used for stamping and path
  construction.

- onefile:

  Logical. If `TRUE` and `plot` is a list, all plots are combined into a
  single multi-page PDF.

- use.names:

  Logical. If `plot` is a list, should `names(plot)` be used in output
  file names rather than numeric indices?

- formats:

  Character vector of file formats, a subset of `c("png","pdf")`.

- canvas:

  Canvas size specification. See
  [`canvasSize`](https://nmautoverse.github.io/tracee/reference/canvasSize.md).

- quiet:

  Logical. If `TRUE`, suppresses messages about files written.

- fun.path:

  Function or character string defining output directory structure. See
  [`pathFormat`](https://nmautoverse.github.io/tracee/reference/pathFormat.md).
  If the path format uses `%script`, `script` is forwarded to it
  automatically.

- subdir:

  Character string, subdirectory passed on to `fun.path`.

- name.all.pdf:

  Character string, file name to use for a combined multi-page PDF when
  `file = ""` and `onefile = TRUE`. Default `"all.pdf"`.

- method.png:

  Character string, one of `"png"` or `"ragg"`, specifying which
  graphics device to use for PNG output. See
  [`write1`](https://nmautoverse.github.io/tracee/reference/write1.md).

- ...:

  Passed on to
  [`write1`](https://nmautoverse.github.io/tracee/reference/write1.md)
  (and ultimately to
  [`png`](https://rdrr.io/r/grDevices/png.html)/[`pdf`](https://rdrr.io/r/grDevices/pdf.html)).

## Value

`NULL`, invisibly. Files are written (and/or plots displayed) as a side
effect.

## See also

[`ggwrite`](https://nmautoverse.github.io/tracee/reference/ggwrite.md),
[`write1`](https://nmautoverse.github.io/tracee/reference/write1.md),
[`ggwrite_names`](https://nmautoverse.github.io/tracee/reference/ggwrite_names.md)
