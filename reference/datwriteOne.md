# Write a single data object to file

Internal function that handles writing a single data frame to file using
NMwriteData. Constructs file paths, prepares stamping arguments, and
calls NMwriteData with appropriate parameters.

## Usage

``` r
datwriteOne(
  x,
  file,
  formats,
  script,
  time,
  model,
  fun.path,
  subdir = NULL,
  quiet = FALSE,
  ...
)
```

## Arguments

- x:

  A data frame to write

- file:

  Character string specifying the output file name

- formats:

  Character vector of output formats

- script:

  Optional character string specifying the script name for stamping

- time:

  Optional time stamp to include in output

- model:

  Optional model object or path for organizing output files

- fun.path:

  Path function as returned by
  [`pathFormat`](https://nmautoverse.github.io/tracee/reference/pathFormat.md),
  or `NULL`.

- subdir:

  Optional subdirectory for organizing output files

- quiet:

  Logical. If `TRUE`, suppresses messages about ignored `model`/`script`
  arguments from `fun.path`.

- ...:

  Additional arguments passed to NMwriteData

## Value

Result from NMwriteData call
