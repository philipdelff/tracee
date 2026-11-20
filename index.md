# tracee

`tracee` makes it easy to save plots, tables and data from R in a way
that can always be traced back to the code that produced them.

When you write an output with `tracee`, you get:

- **A stamp on the output** with the script that generated it, the
  output file path and the creation time (optionally also a model name).
  Anyone who finds the plot in a report can go back to the exact script.
- **Consistent file organization** through simple path formats such as
  `"%model/%name_%model"` or `"%script/%name"`.
- **One interface for several object types.** `ggplot`s, `flextable`s
  and data frames can be written one at a time or as a whole list in a
  single call.
- **Output sizes that suit the purpose.** Use named canvases
  (`"standard"`, `"wide"`, `"wide-screen"`, `"A4"`, …) for slides and
  reports, or your own dimensions.
- **One switch between interactive work and file output.** The same code
  shows outputs on screen while you develop, and writes them to file
  when you set `save = TRUE`.

The project website is at <https://philipdelff.github.io/tracee/>.

## Installation

`tracee` is on CRAN and MPN:

``` r

install.packages("tracee")
```

The development version can be installed from GitHub:

``` r

# install.packages("remotes")
remotes::install_github("philipdelff/tracee")
```

## Overview of functions

| Function | Purpose |
|----|----|
| [`ggwrite()`](https://philipdelff.github.io/tracee/reference/ggwrite.md) | Write `ggplot` objects (or lists of them) to `png`/`pdf` with stamp |
| [`ftwrite()`](https://philipdelff.github.io/tracee/reference/ftwrite.md) | Write `flextable` objects to `png`, `docx`, `pptx`, `html`, `rds` |
| [`datwrite()`](https://philipdelff.github.io/tracee/reference/datwrite.md) | Write data frames (via [`NMdata::NMwriteData()`](https://nmautoverse.github.io/NMdata/reference/NMwriteData.html)) with metadata |
| [`writer()`](https://philipdelff.github.io/tracee/reference/writer.md) | Detect the object type and dispatch to the right writer |
| [`lwrite()`](https://philipdelff.github.io/tracee/reference/lwrite.md) | Write a (nested) list of plots, tables and data in one call |
| [`ggstamp()`](https://philipdelff.github.io/tracee/reference/ggstamp.md), [`ftstamp()`](https://philipdelff.github.io/tracee/reference/ftstamp.md) | Add a traceability stamp without writing to file |
| [`pathFormat()`](https://philipdelff.github.io/tracee/reference/pathFormat.md) | Build output paths from templates using `%model`, `%script`, `%name` |
| [`canvasSize()`](https://philipdelff.github.io/tracee/reference/canvasSize.md) | Predefined output dimensions |
| [`traceit()`](https://philipdelff.github.io/tracee/reference/traceit.md) | Attach script/model/time information to an object for later writing |

## Examples

### Save a plot with a stamp

The device is chosen from the file name extension (`png` or `pdf`).

``` r

library(ggplot2)
library(tracee)

p <- ggplot(ChickWeight, aes(Time, weight, group = Chick)) + geom_line()

ggwrite(p, file = "output/chickweight.png", script = "analysis/plots.R")
```

The plot is saved with a caption showing the script, the output file and
the time of creation.

### Multiple formats and canvas sizes in one call

``` r

ggwrite(p, file = "output/chickweight.png",
        formats = c("png", "pdf"),
        canvas = list(slide = "wide", report = "standard"),
        script = "analysis/plots.R")
# output/chickweight_slide.png, output/chickweight_report.png, and pdf versions
```

### Save a table

``` r

library(flextable)
ft <- flextable(head(ChickWeight))
ftwrite(ft, file = "output/chickweight_table.png", script = "analysis/plots.R")
```

### Organize outputs by model or script

Path formats keep file names consistent across a project. For example,
model-specific outputs can be grouped into a folder per model:

``` r

ggwrite(p, file = "gof.png", model = "run001",
        path.format = "%model/%name_%model",
        script = "analysis/plots.R")
# run001/gof_run001.png
```

The default format is `"%name"`, i.e. `file` is used as is. See
[`?pathFormat`](https://philipdelff.github.io/tracee/reference/pathFormat.md)
for details.

### Write everything at once

Collect the outputs of a script in a list and write them in one go.
Plots, tables and data frames are each sent to the right writer:

``` r

outputs <- list(
  gof   = p,
  table = ft,
  data  = ChickWeight
)

lwrite(outputs, dir = "output", model = "run001",
       path.format = "%model/%name_%model",
       script = "analysis/plots.R")
```

### Switch between viewing and saving

``` r

writeOutput <- FALSE   # set to TRUE when ready to write files
ggwrite(p, file = "output/chickweight.png", save = writeOutput)
```

## Learn more

See the vignette *Writing outputs with tracee*
([`vignette("writing-outputs", package = "tracee")`](https://philipdelff.github.io/tracee/articles/writing-outputs.md))
and the [function
reference](https://philipdelff.github.io/tracee/reference/index.html).
