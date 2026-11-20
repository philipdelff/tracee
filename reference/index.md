# Package index

## Write outputs

Write plots, tables and data to file with traceability stamps.

- [`ggwrite()`](https://philipdelff.github.io/tracee/reference/ggwrite.md)
  : Export ggplot plots (and related objects) to files with stamping
  support
- [`ftwrite()`](https://philipdelff.github.io/tracee/reference/ftwrite.md)
  [`writeFlextab()`](https://philipdelff.github.io/tracee/reference/ftwrite.md)
  : Write flextable objects to one or multiple file formats
- [`datwrite()`](https://philipdelff.github.io/tracee/reference/datwrite.md)
  : Write data to file(s) using NMwriteData
- [`writer()`](https://philipdelff.github.io/tracee/reference/writer.md)
  : Write various R objects to files with flexible formatting
- [`lwrite()`](https://philipdelff.github.io/tracee/reference/lwrite.md)
  : Write lists of outputs with flexible organization and naming

## Stamping and tracing

- [`ggstamp()`](https://philipdelff.github.io/tracee/reference/ggstamp.md)
  : Always stamp your plots with script name
- [`ftstamp()`](https://philipdelff.github.io/tracee/reference/ftstamp.md)
  : Stamp and write flextab objects to one or multiple formats
- [`traceit()`](https://philipdelff.github.io/tracee/reference/traceit.md)
  : Add tracing attributes to R objects for automated output writing

## File paths and dimensions

- [`pathFormat()`](https://philipdelff.github.io/tracee/reference/pathFormat.md)
  : Create a function to derive output file paths from a path format
- [`canvasSize()`](https://philipdelff.github.io/tracee/reference/canvasSize.md)
  : Get predefined canvas sizes

## Utilities

- [`ggwater()`](https://philipdelff.github.io/tracee/reference/ggwater.md)
  : Add watermarks to ggplots
- [`is.gg()`](https://philipdelff.github.io/tracee/reference/is.gg.md) :
  Check if an object is a ggplot or gtable
- [`is.listnotplot()`](https://philipdelff.github.io/tracee/reference/is.listnotplot.md)
  : Check if an object is a list but not a plot, data frame, or
  flextable
- [`padZeros()`](https://philipdelff.github.io/tracee/reference/padZeros.md)
  : Pad Numbers with Leading Zeros
- [`unwrapOutputs()`](https://philipdelff.github.io/tracee/reference/unwrapOutputs.md)
  : Unwrap nested lists of R objects into a flat list of outputs

## Deprecated

- [`pathStruct()`](https://philipdelff.github.io/tracee/reference/pathStruct.md)
  : Deprecated: use pathFormat()
- [`ftwrite()`](https://philipdelff.github.io/tracee/reference/ftwrite.md)
  [`writeFlextab()`](https://philipdelff.github.io/tracee/reference/ftwrite.md)
  : Write flextable objects to one or multiple file formats
- [`stampFlextab()`](https://philipdelff.github.io/tracee/reference/stampFlextab.md)
  : Stamp and write flextab objects (deprecated - use ftstamp)
