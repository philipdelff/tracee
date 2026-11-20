# Resolve path.format (and deprecated fun.path) to a path function

Internal helper used by
[`ggwrite`](https://philipdelff.github.io/tracee/reference/ggwrite.md),
[`ftwrite`](https://philipdelff.github.io/tracee/reference/ftwrite.md),
[`datwrite`](https://philipdelff.github.io/tracee/reference/datwrite.md),
and
[`writer`](https://philipdelff.github.io/tracee/reference/writer.md).

## Usage

``` r
argPathFormat(path.format = NULL, fun.path = NULL)
```

## Arguments

- path.format:

  Character string, function or `NULL`.

- fun.path:

  Deprecated alias for `path.format`.

## Value

A path function as returned by
[`pathFormat`](https://philipdelff.github.io/tracee/reference/pathFormat.md),
or `NULL` if neither argument is given.
