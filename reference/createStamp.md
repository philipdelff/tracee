# Create a stamp with script path, output file path and time stamp in it.

Create a stamp with script path, output file path and time stamp in it.

## Usage

``` r
createStamp(file, script, time, model, addto, stamp.format, stamp.funs = NULL)
```

## Arguments

- file:

  the output file path

- script:

  The script path

- time:

  The default is to insert a time stamp taken from result of Sys.time().
  Using the time argument you can overrule this by setting a fixed
  string instead. Use "" to omit.

- model:

  An optional model name or object to be included in the stamp.

- addto:

  An existing caption to keep above the caption created by this
  function.

- stamp.format:

  A template for the stamp text using the placeholders `%script`,
  `%time`, `%model`, and `%file`. Use `"\n"` for line breaks.
  Placeholders without a value are removed, together with the resulting
  repeated spaces and empty lines. Default is
  `"%script\n%time %model %file"`.

- stamp.funs:

  Optional named list of functions used to preprocess `script`, `time`,
  `model`, and/or `file` before they are substituted into
  `stamp.format`. Each function takes the value as its single argument
  and returns a character string. A supplied function replaces the
  default for that element. Example:
  `stamp.funs = list(script = basename)`.

## Value

A character string, or `NULL` if neither `script` nor `model` is given.
