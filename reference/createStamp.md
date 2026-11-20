# Create a stamp with script path, output file path and time stamp in it.

Create a stamp with script path, output file path and time stamp in it.

## Usage

``` r
createStamp(file, script, time, model, addto)
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

- addto:

  An existing caption to keep above the caption created by this
  function.
