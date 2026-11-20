# Resolve all output file names for ggwrite

Internal function that generates all combinations of file formats and
canvas sizes for
[`ggwrite`](https://philipdelff.github.io/tracee/reference/ggwrite.md).
Returns a `data.table` with columns: format, name.canvas, width, height,
name.file.canvas, row.

## Usage

``` r
ggwrite_names(file, formats, canvas)
```

## Arguments

- file:

  Character string, base file name

- formats:

  Character vector of file formats ("png", "pdf")

- canvas:

  Canvas size specification (see
  [`canvasSize`](https://philipdelff.github.io/tracee/reference/canvasSize.md))

## Value

A `data.table` with one row per format-canvas combination
