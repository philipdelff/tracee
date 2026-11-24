# Add watermarks to ggplots

Add watermarks to ggplots

## Usage

``` r
ggwater(text = "Not validated", scale = 1, rot = 30, col = "grey", alpha = 0.5)
```

## Arguments

- text:

  What should be written on the plot

- scale:

  the size

- rot:

  rotation of the mark. Don't remember the unit.

- col:

  the colour (a value, not an expression) of the watermark. Default is
  grey.

- alpha:

  alpha value for the watermark. Default is 0.5.

## Value

a layer with a watermark that can be added to a plot

## Details

This used to be based on
https://www.r-bloggers.com/adding-watermarks-to-plots/ That solution
stopped working, and this new solution is simpler and based on ggplot2
alone.

## See also

Other Plotting:
[`canvasSize()`](https://nmautoverse.github.io/tracee/reference/canvasSize.md),
[`ggstamp()`](https://nmautoverse.github.io/tracee/reference/ggstamp.md),
[`ggwrite()`](https://nmautoverse.github.io/tracee/reference/ggwrite.md)

## Examples

``` r
library(ggplot2)
ff <- qplot(1:10, 11:20) + ggwater()
#> Warning: `qplot()` was deprecated in ggplot2 3.4.0.
```
