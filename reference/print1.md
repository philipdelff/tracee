# Print a plot or gtable object to the current device

Internal helper that dispatches to the correct printing mechanism
depending on the class of `plot`. `gtable` objects (e.g. from
[`grid.arrange`](https://rdrr.io/pkg/gridExtra/man/arrangeGrob.html))
must be drawn with [`grid.draw`](https://rdrr.io/r/grid/grid.draw.html),
while other objects (e.g. `ggplot`) use the standard `print` method.
`NULL` input is silently ignored.

## Usage

``` r
print1(plot)
```

## Arguments

- plot:

  A plot object (`ggplot`, `gtable`, or similar), or `NULL`.

## Value

`NULL`, invisibly. Called for the side effect of drawing to the current
device.
