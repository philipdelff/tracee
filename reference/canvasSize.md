# Get predefined canvas sizes

Get predefined canvas sizes

## Usage

``` r
canvasSize(canvas, scale = 1, simplify = TRUE)
```

## Arguments

- canvas:

  Can either be a character with one of the values

  - "standard" A powerpoint standard size, and two can be shown side by
    side - 7.2 by 5.4 in (WxH).

  - "wide" For a single wide plot on a powerpoint slide - 9.6 by 5.4 in

  - "A4" A full A4 page - 5.4 by 7.2 in

  - "square" As reads - 5.4 by 5.4 in

  - "wide-screen" For full screen display - 18.6 by 9 in

  or it can be a list with elements width and height with single values
  (unit is inches). Example: canvas=list(height=5,width=9).

- scale:

  A scale to apply to both directions of the canvas size. This can be
  useful in combination with the pre-defined canvas sizes.

- simplify:

  If only one canvas returned and simplify=TRUE, the result will not be
  wrapped in a list. For programming, you most likely want
  simplify=FALSE to be sure to always get the same format back.

## Value

A list with numerical elements width and height as used by ggwrite.

## See also

Other Plotting:
[`ggstamp()`](https://philipdelff.github.io/tracee/reference/ggstamp.md),
[`ggwater()`](https://philipdelff.github.io/tracee/reference/ggwater.md),
[`ggwrite()`](https://philipdelff.github.io/tracee/reference/ggwrite.md)
