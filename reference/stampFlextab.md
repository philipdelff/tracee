# Stamp and write flextab objects (deprecated - use ftstamp)

Deprecated - use
[`ftstamp()`](https://philipdelff.github.io/tracee/reference/ftstamp.md)
instead.

## Usage

``` r
stampFlextab(ft, file, script, bg = "#ffffff", time)
```

## Arguments

- ft:

  a flextable object

- file:

  The file that the flextable will be written to (no file is written by
  this function)

- script:

  path to script - will be pasted as caption.

- bg:

  Default bacground colour is \#ffffff.

- time:

  The default is to insert a time stamp taken from result of Sys.time().
  Using the time argument you can overrule this by setting a fixed
  string instead. Use "" to omit.
