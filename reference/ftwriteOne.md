# Write a single flextable object to file(s)

Internal function called by
[`ftwrite`](https://philipdelff.github.io/tracee/reference/ftwrite.md)
to handle writing of individual flextable objects. Generally not called
directly by users.

## Usage

``` r
ftwriteOne(
  ft,
  file,
  formats,
  save = TRUE,
  show = !save,
  quiet = FALSE,
  script,
  time,
  model,
  format.stamp,
  fun.path,
  subdir
)
```

## Arguments

- ft:

  A `flextable` object to write.

- file:

  Character string, base file name (without extension).

- formats:

  Character vector of output format(s). See
  [`ftwrite`](https://philipdelff.github.io/tracee/reference/ftwrite.md).

- save:

  Logical, whether to save to file. Default `TRUE`.

- show:

  Logical, whether to display the table. Default `!save`.

- quiet:

  Logical, suppress messages. Default `FALSE`.

- script:

  Character string, path to script for stamping.

- time:

  Character string or POSIXct, timestamp for stamping.

- model:

  Character string or model object, model identifier for stamping.

- format.stamp:

  Format specification for stamp. Passed to
  [`ftstamp`](https://philipdelff.github.io/tracee/reference/ftstamp.md).

- fun.path:

  Function for constructing file paths. See
  [`pathFormat`](https://philipdelff.github.io/tracee/reference/pathFormat.md).

- subdir:

  Character string, subdirectory for outputs.

## Value

If `show = TRUE`, returns the flextable object (possibly stamped).
Otherwise returns invisibly a list of file paths written.

## See also

[`ftwrite`](https://philipdelff.github.io/tracee/reference/ftwrite.md)
