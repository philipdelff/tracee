# Stamp and write flextab objects to one or multiple formats

Stamp and write flextab objects to one or multiple formats

## Usage

``` r
ftstamp(ft, file, script, time, model, format.stamp)
```

## Arguments

- ft:

  a flextable object

- file:

  The file that the flextable will be written to (no file is written by
  this function)

- script:

  path to script - will be pasted as caption.

- time:

  The default is to insert a time stamp taken from result of Sys.time().
  Using the time argument you can overrule this by setting a fixed
  string instead. Use "" to omit.

- model:

  Character string or model object specifying the model name/identifier.
  Used for stamping. If a list (model object) is provided, the `$label`
  element is extracted.

- format.stamp:

  A list specifying formatting options for the stamp. Valid elements
  include: `size` (font size, default 4), `align` (text alignment,
  default "right"), `space` (line spacing, default 1), `bg` (background
  color, default "#ffffff"), and `color` (text color, default
  "#666666"). Any unspecified elements use the defaults.
