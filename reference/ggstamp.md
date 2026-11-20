# Always stamp your plots with script name

This function is used to stamp ggplot type plots with datetime and
script name. User must provide the script name.

## Usage

``` r
ggstamp(plot, file, size, script, time, model, format.stamp)
```

## Arguments

- plot:

  The plot to be stamped.

- file:

  An optional output filename to be included in the stamp.

- size:

  The size (list) for the plot, not the caption. Used to calculate the
  size of the caption.

- script:

  the script name. Date and time will be added automatically.

- time:

  The timestamp to be included.

- model:

  An optional model name or object to be included in the stamp.

- format.stamp:

  A list of formatting options for the stamp text. For ggplot objects,
  valid elements include size, colour, hjust, vjust, and other
  parameters accepted by
  [`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html).
  For gtable objects, valid elements include col, fill, alpha, lty, lwd,
  lex, lineend, linejoin, linemitre, fontsize, cex, fontfamily,
  fontface, lineheight, and font. Default formatting is applied if not
  specified.

## Value

the plot with a stamp

A plot object with the stamp added as caption

## Details

The stamp is adding using the caption label. If a caption is already in
the plot, the stamp will be added in a new line.

The caption is derived as
caption=paste(c(plot\$label\$caption,stamp,paste(date.txt,file)),collapse="\n")

ggplot 2.2.1 (which is years old) or newer is required.

## See also

Other Plotting:
[`canvasSize()`](https://philipdelff.github.io/tracee/reference/canvasSize.md),
[`ggwater()`](https://philipdelff.github.io/tracee/reference/ggwater.md),
[`ggwrite()`](https://philipdelff.github.io/tracee/reference/ggwrite.md)

## Examples

``` r
library(ggplot2)
data(ChickWeight)
p1 <- ggplot(ChickWeight,aes(Time,weight,group=Chick,colour=factor(Diet)))+geom_line()
script <- "note"
ggstamp(p1,script)

## Or use ggwrite which will call ggstamp when the `script` argument is provided.
ggwrite(p1,script=script,canvas="wide")
```
