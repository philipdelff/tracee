# Write a single plot to a single file (or display it)

Internal function that performs the actual device handling (opening a
[`png`](https://rdrr.io/r/grDevices/png.html)/[`agg_png`](https://ragg.r-lib.org/reference/agg_png.html)
or [`pdf`](https://rdrr.io/r/grDevices/pdf.html) device, printing the
plot, and closing the device) for a single plot/file/size combination.
Also applies stamping via
[`ggstamp`](https://philipdelff.github.io/tracee/reference/ggstamp.md)
before printing. If `fn` or `type` indicate on-screen display ("x11"),
the plot is printed directly without opening a file device.

## Usage

``` r
write1(
  plot,
  fn = NULL,
  type = NULL,
  onefile = FALSE,
  size,
  script,
  time,
  model,
  quiet = FALSE,
  subdir = NULL,
  method.png = c("ragg", "png"),
  fun.path = NULL,
  stamp.format = NULL,
  stamp.funs = NULL,
  ...
)
```

## Arguments

- plot:

  A single plot object (`ggplot`, `gtable`, or similar).

- fn:

  Character string, output file name (without or with extension; the
  extension is replaced/added based on `type`). `NULL` or `""` to only
  display on screen.

- type:

  Character string, one of `"png"`, `"pdf"`, or `"x11"` (display only,
  no file written).

- onefile:

  Logical, passed to the `pdf` device via `dots` when writing multiple
  pages to a single PDF file. Not directly used for the `png` device.

- size:

  A list with `width` and `height` elements (in inches), as produced by
  [`canvasSize`](https://philipdelff.github.io/tracee/reference/canvasSize.md)/[`ggwrite_names`](https://philipdelff.github.io/tracee/reference/ggwrite_names.md).

- script:

  Character string or path to the calling script, used for stamping. See
  [`ggstamp`](https://philipdelff.github.io/tracee/reference/ggstamp.md).

- time:

  Character string or POSIXct timestamp used for stamping.

- model:

  Character string or model object used for stamping and path
  construction.

- quiet:

  Logical. If `TRUE`, suppresses the "Written to ..." message.

- subdir:

  Character string, subdirectory passed on to `fun.path`.

- method.png:

  Character string, one of `"png"` or `"ragg"`. Controls whether
  [`png`](https://rdrr.io/r/grDevices/png.html) or
  [`agg_png`](https://ragg.r-lib.org/reference/agg_png.html) is used as
  the PNG graphics device. `ragg` produces output that is more
  consistent across platforms.

- fun.path:

  Function or character string defining output directory structure,
  applied to `fn` before the file is written. See
  [`pathFormat`](https://philipdelff.github.io/tracee/reference/pathFormat.md).
  If the path format uses `%script`, `script` is forwarded to it
  automatically (if the function accepts a `script` argument).

- stamp.format, stamp.funs:

  Passed to
  [`ggstamp`](https://philipdelff.github.io/tracee/reference/ggstamp.md).

- ...:

  Additional arguments. `res` is passed on to the `png`/ `ragg` device,
  `onefile` to the `pdf` device. A `dir` element (if present) is
  extracted but currently only used as a default working directory. A
  `fun.path` element (if present) overrides the `fun.path` argument.

## Value

`NULL`, invisibly. Called for the side effect of writing a file and/or
displaying the plot.

## See also

[`ggwrite`](https://philipdelff.github.io/tracee/reference/ggwrite.md),
[`writeObj`](https://philipdelff.github.io/tracee/reference/writeObj.md),
[`ggstamp`](https://philipdelff.github.io/tracee/reference/ggstamp.md)
