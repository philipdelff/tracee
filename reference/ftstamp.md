# Stamp and write flextab objects to one or multiple formats

Stamp and write flextab objects to one or multiple formats

## Usage

``` r
ftstamp(
  ft,
  file,
  script,
  time,
  model,
  stamp.ft.style,
  stamp.format = NULL,
  stamp.funs = NULL
)
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

- stamp.ft.style:

  A list specifying formatting options for the stamp. Valid elements
  include: `size` (font size, default 4), `align` (text alignment,
  default "right"), `space` (line spacing, default 1), `bg` (background
  color, default "#ffffff"), and `color` (text color, default
  "#666666"). Any unspecified elements use the defaults.

- stamp.format:

  A template for the stamp text using the placeholders `%script`,
  `%time`, `%model`, and `%file`, with `"\n"` for line breaks. Default
  is `"%file\n%script\n%model\n%time"`. See
  [`createStamp`](https://nmautoverse.github.io/tracee/reference/createStamp.md).

- stamp.funs:

  Optional named list of functions (elements `script`, `time`, `model`,
  `file`) used to preprocess the values before they are inserted into
  `stamp.format`. See
  [`createStamp`](https://nmautoverse.github.io/tracee/reference/createStamp.md).

## Value

A flextable object with the stamp added as a footer line.
