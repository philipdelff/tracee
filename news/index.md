# Changelog

## tracee 0.1.0

CRAN release: 2026-11-24

This version comes with several improvements, new functions and
bugfixes. While previous versions provided functions to save to tables
and figures with stamps, version 0.1.0 improves the consistency of those
functions, adds a consistent framework to define file path and naming
based on output name, model name and script name. This means one can
freely define if outputs should be saved in folders and with file names
based on a model name and or a script name. If saving a plot object,
$`p`$, this could look like

``` r

ggwrite(p,file="myplot.png",model="models/mymodel.lst",script="myscript.R",path.format="outputs/%script/%model_%name")
```

this would generate a file in “outputs/myscript/mymodel_myplot.png”
which will include a “stamp” in the bottom with script, model, and
output name information.

You can also use the
[`lwrite()`](https://nmautoverse.github.io/tracee/reference/lwrite.md)
(“list write”) function to achieve this for a set of outputs which can
be a mix of figures, tables, and data.frames. Those would then be
dispatched to relevant writers.

Suppose `p` is a plot, and `ft` is a flextable object.

``` r

lwrite(list(myplot=p,mytab=ft),
path.format="outputs/%script/%model_%name",
,model="models/mymodel.lst",script="myscript.R")
```

would create the two files “outputs/myscript/myplot_mymodel.png” and
“outputs/myscript/mytab_mymodel.png”

### New Features

This list of improvements is far from exhaustive.

- A `model` argument is generally added to writer functions to allow for
  model information in tags.

- Stamping mechanism has been re-written and greatly improved. Stamps
  format (what to write) is controlled by `stamp.format` (very similar
  to `path.format` for file paths) and styling (font etc.) is controlled
  by `stamp.gg.style` (plots) and `stamp.ft.style` (flextables). The
  default is `stamp.format="%file\n%script\n%model\n%time"` with labels
  added by the default `stamp.funs` (“Output:”, “Script:”, “Model:”,
  “Time:”). Components that are not provided are skipped.
  `stamp.gg.style` is a list of arguments passed to `element_text`.

- Plot stamps are now left-justified by default and placed in the bottom
  right corner. Placement and justification are controlled independently
  by `stamp.gg.style$position` (`"left"`, `"center"`, `"right"` or a
  number between 0 and 1) and `stamp.gg.style$hjust`.

- Generation of stamps have been streamlined between
  [`ggwrite()`](https://nmautoverse.github.io/tracee/reference/ggwrite.md)
  and
  [`ftwrite()`](https://nmautoverse.github.io/tracee/reference/ftwrite.md),
  and they are now using the same underlying mechanism for
  simplification.

- [`ftwrite()`](https://nmautoverse.github.io/tracee/reference/ftwrite.md)
  gains the `show` argument. Similarly to
  [`ggwrite()`](https://nmautoverse.github.io/tracee/reference/ggwrite.md),
  this allows to control printing to file and screen independently.

- [`ggwrite()`](https://nmautoverse.github.io/tracee/reference/ggwrite.md)
  (and wrappers) gains support for the ragg device which is now default.
  This improves reproducibility across systems.

- [`traceit()`](https://nmautoverse.github.io/tracee/reference/traceit.md)
  can be used to modify saving options for individual outputs that are
  saved in lists and written at once with
  [`lwrite()`](https://nmautoverse.github.io/tracee/reference/lwrite.md).
  The options chosen with
  [`traceit()`](https://nmautoverse.github.io/tracee/reference/traceit.md)
  will then overwrite defaults for the specific plots.

- arguments like `model` and `script` can be pre-prossed before used in
  paths and stamps. Like when using `path.format="%script-%name"`, one
  may want to truncate the script path or name before generating the
  file name. This can be done independently for generation of paths
  (`paths.funs`) and stamps (`stamps.funs`).

### Bugfixes

- [`ftwrite()`](https://nmautoverse.github.io/tracee/reference/ftwrite.md)
  would need the `bg` argument even though a default value was
  apparently provided. Now that default is actually being used.

### Other changes

- [`pathStruct()`](https://nmautoverse.github.io/tracee/reference/pathStruct.md)
  has been renamed to
  [`pathFormat()`](https://nmautoverse.github.io/tracee/reference/pathFormat.md).
  [`pathStruct()`](https://nmautoverse.github.io/tracee/reference/pathStruct.md)
  is deprecated and calls
  [`pathFormat()`](https://nmautoverse.github.io/tracee/reference/pathFormat.md)
  with a message.
- The `structure` argument of
  [`pathFormat()`](https://nmautoverse.github.io/tracee/reference/pathFormat.md)
  and
  [`lwrite()`](https://nmautoverse.github.io/tracee/reference/lwrite.md)
  has been renamed to `path.format`. `structure` is deprecated but still
  works (with a message).
- `ggwrite` gives a message instead of a warning when empty spaces are
  being removed from file names.

## tracee 0.0.4

CRAN release: 2024-01-08

### Bugfixes

- [`ggwrite()`](https://nmautoverse.github.io/tracee/reference/ggwrite.md)
  in some cases wouldn’t write a list of plots to multiple pngs. Fixed.

### Other changes

- `ftwrite` argument takes a `time` argument like `ggwrite`. It did
  already by passing it downstream but now it is explicit like for
  `ggwrite`.

## tracee 0.0.3

CRAN release: 2023-03-23

### Changes

- Flextab function names aligned with functions for plots. writeFlextab
  is deprecated and now called ftwrite. stampFlextab is deprecated and
  now called ftstamp. These names match ggwrite and ggstamp,
  respectively.

### New features

- Missing script argument supported in ftwrite.

- ggwrite support multiple formats (new argument aligning with
  writeFlextab) and canvases. In this case, the canvas name will be
  appended to file names.

- ftwrite now throws an error in case unsupported output formats are
  requested.

- ftwrite support the ‘save’ argument like ggwrite.

## tracee 0.0.2

CRAN release: 2022-10-15

### New functions

- writeFlextab is a wrapper that flextable objects to files including a
  caption (stamp) referencing script path and output filename. The stamp
  is generated by the same underlying function as used when generating
  stamps for plots in ggstamp or ggwrite. stampFlextab is equivalent to
  ggstamp for plots.

### Added features

- Objets of class ggmatrix can now be stamped using ggstamp.
