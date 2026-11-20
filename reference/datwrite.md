# Write data to file(s) using NMwriteData

This function writes data objects to files, with support for lists of
data objects. It wraps NMwriteData and provides additional functionality
for file path construction and metadata stamping.

## Usage

``` r
datwrite(
  x,
  file,
  script,
  time,
  model,
  formats,
  path.format = NULL,
  subdir = NULL,
  quiet = FALSE,
  fun.path = NULL,
  ...
)
```

## Arguments

- x:

  A data frame or list of data frames to write

- file:

  Character string specifying the output file name

- script:

  Optional character string specifying the script name for stamping

- time:

  Optional time stamp to include in output

- model:

  Optional model object or path for organizing output files

- formats:

  Character vector of output formats (passed to NMwriteData)

- path.format:

  Character string or function defining the output path. Passed to
  [`pathFormat`](https://philipdelff.github.io/tracee/reference/pathFormat.md).
  Default (`NULL`) uses `file` as is.

- subdir:

  Optional subdirectory for organizing output files

- quiet:

  Logical. If `TRUE`, suppresses messages about ignored `model`/`script`
  arguments from `path.format`.

- fun.path:

  Deprecated. Use `path.format`.

- ...:

  Additional arguments passed to NMwriteData

## Value

A list of results from NMwriteData calls
