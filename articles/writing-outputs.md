# Writing Outputs with ggwrite, ftwrite, and lwrite

## Learning objectives

By the end of this vignette you will be able to:

- Use
  [`ggwrite()`](https://philipdelff.github.io/tracee/reference/ggwrite.md)
  to save `ggplot` objects (single plots or lists of plots) to PNG
  and/or PDF files.
- Use
  [`ftwrite()`](https://philipdelff.github.io/tracee/reference/ftwrite.md)
  to save `flextable` objects to PNG, HTML, DOCX, PPTX, or RDS files,
  including writing a named list of tables to multiple files at once.
- Use
  [`lwrite()`](https://philipdelff.github.io/tracee/reference/lwrite.md)
  to save a *list of mixed objects* (plots, tables, etc.) in one call,
  dispatching each element to the appropriate writer function.
- Add a **stamp** to your outputs using the `script`, `time`, and
  `model` arguments, so every figure/table clearly shows which script
  produced it, when, and for which model.
- Control output behavior globally using a flag variable
  (e.g. `writeOutput`), so the same script can be used interactively
  (viewing results on screen) and in batch mode (saving all outputs to
  disk).
- Organize output files using `fun.path`/`subdir`, so outputs are
  written into a structured directory tree rather than a single flat
  folder.

``` r

library(tracee)
library(ggplot2)
library(flextable)
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following object is masked from 'package:base':
#> 
#>     %notin%

## Flip this to TRUE to actually write files to disk when running this
## vignette interactively.
writeOutput <- FALSE

## Arguments used throughout this vignette to "stamp" outputs - see the
## Stamping section below.
script <- "writing-outputs.Rmd"
time   <- "2024-01-15 10:00"
model  <- "run001"
```

## 1. `ggwrite()`: saving (and viewing) plots

[`ggwrite()`](https://philipdelff.github.io/tracee/reference/ggwrite.md)
writes a single plot, or a list of plots, to one or more files. If
`save = FALSE` the plot is only displayed (useful during interactive
development); if `save = TRUE`, it is written to disk.

### A single plot

``` r

data(ChickWeight)

p1 <- ggplot(ChickWeight, aes(Time, weight, group = Chick, colour = factor(Diet))) +
  geom_line() +
  labs(colour = "Diet")

## View on screen (save = FALSE is the default behavior we use throughout
## this vignette via `writeOutput`)
ggwrite(p1, file = "chickweight_lines1.png", save = writeOutput)
```

![Example plot of ChickWeight data saved with tracee, with a stamp
showing output file, script and time
underneath.](writing-outputs_files/figure-html/ggwrite-single-1.png)

### Stamping: `script`, `time`, and `model`

Passing `script` adds a caption (via
[`ggstamp()`](https://philipdelff.github.io/tracee/reference/ggstamp.md))
to the plot, showing the script name, a timestamp, and - if supplied -
the model identifier. This is invaluable for traceability: anyone
looking at a saved figure can immediately see which script, run, and
model produced it.

- `script`: character string identifying the script that generated the
  plot.
- `time`: a timestamp string. If omitted while `script` is supplied, the
  current time is used automatically; pass `time = ""` to omit the
  timestamp from the stamp entirely.
- `model`: a model name/identifier, included in the stamp and also
  usable by `fun.path` to organize output directories (see the final
  section below).

``` r

ggwrite(p1, script = script, file = "chickweight_lines2.png",
        canvas = "wide", save = writeOutput)
```

![Example plot of ChickWeight data saved with tracee, with a stamp
showing output file, script and time
underneath.](writing-outputs_files/figure-html/ggwrite-stamp-1.png)

``` r


## Including time and model in the stamp
ggwrite(p1, script = script, time = time, model = model,
        file = "chickweight_lines3.png", save = writeOutput)
```

![Example plot of ChickWeight data saved with tracee, with a stamp
showing output file, script and time
underneath.](writing-outputs_files/figure-html/ggwrite-stamp-2.png)

### Multiple formats and canvas sizes at once

``` r

## Two file formats from one call
ggwrite(p1, file = "chickweight_lines4.png",
        formats = c("png", "pdf"), save = writeOutput)
```

![Example plot of ChickWeight data saved with tracee, with a stamp
showing output file, script and time
underneath.](writing-outputs_files/figure-html/ggwrite-formats-canvas-1.png)

``` r


## Two canvas sizes from one call -> chickweight_small.png, chickweight_large.png
ggwrite(p1, file = "chickweight_lines5.png",
        canvas = list(small = "standard", large = "wide"),
        save = writeOutput)
```

![Example plot of ChickWeight data saved with tracee, with a stamp
showing output file, script and time
underneath.](writing-outputs_files/figure-html/ggwrite-formats-canvas-2.png)

### Lists of plots

A named list of plots can be written either as separate files (one per
element) or combined into a single multi-page PDF.

``` r

p2 <- ggplot(ChickWeight, aes(Time, weight)) +
  geom_point(alpha = 0.3)

plots <- list(lines = p1, points = p2)

## Separate files: chickweight_lines.png, chickweight_points.png
ggwrite(plots, file = "chickweight.png", use.names = TRUE, save = writeOutput)
```

![Example plot of ChickWeight data saved with tracee, with a stamp
showing output file, script and time
underneath.](writing-outputs_files/figure-html/ggwrite-list-1.png)![Example
plot of ChickWeight data saved with tracee, with a stamp showing output
file, script and time
underneath.](writing-outputs_files/figure-html/ggwrite-list-2.png)

``` r


## Single multi-page PDF: chickweight.pdf (2 pages)
ggwrite(plots, file = "chickweight.pdf", onefile = TRUE, save = writeOutput)
```

![Example plot of ChickWeight data saved with tracee, with a stamp
showing output file, script and time
underneath.](writing-outputs_files/figure-html/ggwrite-list-3.png)![Example
plot of ChickWeight data saved with tracee, with a stamp showing output
file, script and time
underneath.](writing-outputs_files/figure-html/ggwrite-list-4.png)

## 2. `ftwrite()`: saving flextable objects

[`ftwrite()`](https://philipdelff.github.io/tracee/reference/ftwrite.md)
writes `flextable` tables to common document/image formats: `png`,
`html`, `docx`, `pptx`, or `rds`. Like
[`ggwrite()`](https://philipdelff.github.io/tracee/reference/ggwrite.md),
it accepts a single `flextable` object or a named list of them.

``` r

ft1 <- flextable(head(ChickWeight, 10))

## Write a single table, format inferred from the file extension
ftwrite(ft1, file = "chickweight_table.docx", save = writeOutput)
```

### Stamping a table with `script`, `time`, and `model`

Just like
[`ggwrite()`](https://philipdelff.github.io/tracee/reference/ggwrite.md),
[`ftwrite()`](https://philipdelff.github.io/tracee/reference/ftwrite.md)
accepts `script`, `time`, and `model` to add a traceability stamp to the
output.

``` r

ftwrite(ft1, script = script, time = time, model = model,
        file = "chickweight_table_1.docx", save = writeOutput)
```

### Multiple formats

``` r

ftwrite(ft1, file = "chickweight_table_2.docx",
        formats = c("docx", "html"), save = writeOutput)
```

### A list of tables

``` r

ft2 <- flextable(
as.data.frame(summary(ChickWeight))
[, 1:2])

ft2 <- flextable(
as.data.frame(ChickWeight[1:2,1:2]))

ftables <- list(raw = ft1, summary = ft2)

lapply(ftables,class)
#> $raw
#> [1] "flextable"
#> 
#> $summary
#> [1] "flextable"

## Creates chickweight_tables_raw.docx, chickweight_tables_summary.docx
ftwrite(tables, file = "chickweight_tables_3.docx", save = writeOutput)
```

Note: writing to `png` requires a working headless-browser backend
(`webshot2`/`chromote`). If that is not available in your environment,
prefer `html` or `docx` output, which rely only on pure-R packages.

## 3. `lwrite()`: writing a list of mixed objects in one call

Often an analysis produces a *mixture* of outputs - some plots, some
tables - that you want to save together.
[`lwrite()`](https://philipdelff.github.io/tracee/reference/lwrite.md)
takes a named list and dispatches each element to the appropriate writer
([`ggwrite()`](https://philipdelff.github.io/tracee/reference/ggwrite.md)
for plots,
[`ftwrite()`](https://philipdelff.github.io/tracee/reference/ftwrite.md)
for `flextable` objects, and so on), using the list names to generate
file names.

``` r

outputs <- list(
  weight_trajectories = p1,
  weight_by_time      = p2,
  summary_table       = ft2
)

## Each element is written using the function appropriate for its class,
## with file names derived from the list names, stamped consistently with
## script, time, and model.
lwrite(outputs, 
       script = script, time = time, model = model,
       save = writeOutput)
```

![Example plot of ChickWeight data saved with tracee, with a stamp
showing output file, script and time
underneath.](writing-outputs_files/figure-html/lwrite-example-1.png)![Example
plot of ChickWeight data saved with tracee, with a stamp showing output
file, script and time
underneath.](writing-outputs_files/figure-html/lwrite-example-2.png)

This is particularly useful at the end of an analysis script: collect
every plot and table you want to export into a single named list, then
call
[`lwrite()`](https://philipdelff.github.io/tracee/reference/lwrite.md)
once instead of calling
[`ggwrite()`](https://philipdelff.github.io/tracee/reference/ggwrite.md)/[`ftwrite()`](https://philipdelff.github.io/tracee/reference/ftwrite.md)
separately for each object.

## 4. Controlling output location

[`ggwrite()`](https://philipdelff.github.io/tracee/reference/ggwrite.md)
and
[`ftwrite()`](https://philipdelff.github.io/tracee/reference/ftwrite.md)
accept `fun.path`/`subdir` arguments, and
[`lwrite()`](https://philipdelff.github.io/tracee/reference/lwrite.md)
accepts `path.format`. These let you organize outputs into a structured
directory tree (e.g. grouped by model or script) rather than a single
flat folder. A path format is a template using the placeholders
`%model`, `%script` and `%name`, where `/` separates directory levels.
See
[`?pathFormat`](https://philipdelff.github.io/tracee/reference/pathFormat.md)
for details and for how to supply a custom path function.

``` r

ggwrite(p1, file = "chickweight_lines_06.png",
        script = script, time = time, model = model,
        subdir = "exploratory",
        path.format = "%model-%name", save = writeOutput)

lwrite(outputs, dir = "outputs",
       script = script, time = time, model = model,
       path.format = "%model/%model-%name")
```

## Summary

| Function | Object type(s) | Formats |
|----|----|----|
| `ggwrite` | `ggplot`, `gtable`, list thereof | `png`, `pdf` |
| `ftwrite` | `flextable`, list thereof | `png`, `html`, `docx`, `pptx`, `rds` |
| `lwrite` | Named list of mixed objects | Dispatches to `ggwrite`/`ftwrite` |

Using a single `writeOutput` flag (or similar) to control the
`save`/`show` arguments across all three functions lets you switch
seamlessly between interactive exploration and reproducible batch output
generation.
