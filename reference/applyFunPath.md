# Call a path function safely, forwarding script only if supported

Internal helper used by
[`ggwrite`](https://philipdelff.github.io/tracee/reference/ggwrite.md),
[`ftwrite`](https://philipdelff.github.io/tracee/reference/ftwrite.md),
[`datwrite`](https://philipdelff.github.io/tracee/reference/datwrite.md),
and [`lwrite`](https://philipdelff.github.io/tracee/reference/lwrite.md)
to call a `fun.path` function (as returned by
[`pathFormat`](https://philipdelff.github.io/tracee/reference/pathFormat.md),
or user-supplied) with `name`, `model`, `subdir`, and (if supported)
`script`. This avoids "unused argument" errors when a custom `fun.path`
function does not accept a `script` argument.

## Usage

``` r
applyFunPath(
  fun.path,
  name,
  model = NULL,
  script = NULL,
  subdir = NULL,
  quiet = FALSE
)
```

## Arguments

- fun.path:

  A function as returned by
  [`pathFormat`](https://philipdelff.github.io/tracee/reference/pathFormat.md),
  or any custom function accepting at least `name`, `model`, and
  `subdir` arguments.

- name:

  Character string, file name passed to `fun.path`.

- model:

  Model identifier passed to `fun.path`.

- script:

  Script identifier. Only forwarded to `fun.path` if it has a `script`
  formal argument.

- subdir:

  Subdirectory passed to `fun.path`.

- quiet:

  Logical. If `TRUE`, suppresses messages about ignored `model`/`script`
  arguments. Only forwarded to `fun.path` if it has a `quiet` formal
  argument.

## Value

The result of calling `fun.path` with the applicable arguments.
