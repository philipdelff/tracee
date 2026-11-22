# Create a function to derive output file paths from a path format

Generates a function that constructs file paths according to a path
format (a template with placeholders). This is particularly useful in
modeling workflows where outputs need to be organized by model name,
with consistent naming conventions across files.

## Usage

``` r
pathFormat(
  path.format = "%name",
  dir,
  model = NULL,
  script = NULL,
  path.funs = NULL,
  structure
)
```

## Arguments

- path.format:

  A character string specifying a path format template, or a custom
  function. If a function is provided, it is returned without
  modification.

  Templates are specified using `%model`, `%script`, and `%name`
  placeholders, separated by `/` to indicate directory levels. The last
  `/`-separated component is treated as the file name pattern; any
  earlier components are directory names. `%model` is replaced by the
  model name (with any file extension stripped), `%script` by the script
  name (with any file extension stripped), and `%name` by `name` (also
  with its extension stripped, re-attached at the end). Any other
  characters (e.g. `-`, `_`) are kept as literal separators. Examples:

  `"%model/%name_%model"`

  :   Creates paths like `model_name/file_model_name.ext`. Example:
      `"run001/gof_run001.png"`.

  `"%name_%model"`

  :   Creates paths like `file_model_name.ext` with no model
      subdirectory. Example: `"gof_run001.png"`

  `"%model/%model-%name"`

  :   Creates paths like `model_name/model_name-file.ext`. Example:
      `"run001/run001-gof.png"`

  `"%model-%name"`

  :   Creates paths like `model_name-file.ext` with no model
      subdirectory. Example: `"run001-gof.png"`

  `"%script/%name_%model"`

  :   Creates paths like `script_name/file_model_name.ext`. Example:
      `"myscript/gof_run001.png"`

  Any other combination of `%model`, `%script`, `%name`, literal text,
  and `/` is supported - e.g. `"archive/%model/plots/%name"`. Default is
  `"%name"`, i.e. `name` is used as is (relative to `dir`).

- dir:

  Character string specifying the base directory for all outputs. This
  is prepended to all generated paths. Default is `NULL` (current
  directory).

- model:

  Default value for the `model` argument of the generated function. Used
  whenever the generated function is called without explicitly supplying
  `model`. Default is `NULL`.

- script:

  Default value for the `script` argument of the generated function.
  Used whenever the generated function is called without explicitly
  supplying `script`. Default is `NULL`.

- path.funs:

  Optional named list of functions used to preprocess `model` and/or
  `script` before they are substituted into `path.format`. Supported
  names are `model` and `script`. Each function takes the value as its
  single argument and returns a character string. Elements not supplied
  keep the default, which is `function(x) fnExtension(basename(x), "")`
  (strip directory and file extension). Note that a supplied function
  replaces the default entirely, so e.g. extension stripping must be
  included if still wanted. Example:
  `path.funs = list(script = function(x) substr(basename(x), 1, 4))`.
  Default is `NULL`.

- structure:

  Deprecated. Use `path.format`.

## Value

A function with signature
`function(name, model, script, subdir, dir, quiet = FALSE)` that
generates file paths according to the specified path format. If
`path.format` is already a function, it is returned unchanged.

## Details

The returned function accepts the following arguments:

- `name`:

  The base file name (typically without extension)

- `model`:

  A model identifier (character string or model object). If a model
  object (list) is provided, the `$mod` element is extracted and used as
  the model name. The file extension is automatically removed from the
  model name (e.g., "model.lst" becomes "model").

- `script`:

  A script identifier (character string, typically a script file name).
  The file extension is automatically removed (e.g., "myscript.R"
  becomes "myscript").

- `subdir`:

  Optional subdirectory. Inserted between the directory components of
  `path.format` and the file name component.

- `dir`:

  Base directory. Overrides the `dir` argument passed to `pathFormat`.

- `quiet`:

  If `TRUE`, suppress messages about ignored `model`/`script` arguments.
  Default `FALSE`.

The generated function handles missing `model`/`script` arguments
gracefully - if `path.format` does not contain `%model` (or `%script`),
the corresponding argument is optional and ignored (with a message if
supplied anyway). If `path.format` does contain `%model` (or `%script`),
the corresponding argument is required.

## See also

[`writer`](https://philipdelff.github.io/tracee/reference/writer.md) for
using path formats in writing functions,
[`ggwrite`](https://philipdelff.github.io/tracee/reference/ggwrite.md),
[`ftwrite`](https://philipdelff.github.io/tracee/reference/ftwrite.md),
[`lwrite`](https://philipdelff.github.io/tracee/reference/lwrite.md) for
functions that accept `fun.path` arguments

## Examples

``` r
# Create a path function (default format)
path_fun <- pathFormat(path.format = "%model/%name_%model")

# Use it to generate paths
path_fun(name = "gof1.png", model = "run001")
#> [1] "run001/gof1_run001.png"
# Returns: "run001/gof1_run001.png"

path_fun(name = "gof1.png", model = "run001", subdir = "diagnostics")
#> [1] "run001/diagnostics/gof1_run001.png"
# Returns: "run001/diagnostics/gof1_run001.png"

# With base directory
path_fun2 <- pathFormat(path.format = "%model/%model-%name", dir = "outputs")
path_fun2(name = "gof1.png", model = "run001")
#> [1] "outputs/run001/run001-gof1.png"
# Returns: "outputs/run001/run001-gof1.png"

# Different format
path_fun3 <- pathFormat(path.format = "%model-%name")
path_fun3(name = "gof1.png", model = "run001")
#> [1] "run001-gof1.png"
# Returns: "run001-gof1.png"

# Fully custom format
path_fun3b <- pathFormat(path.format = "archive/%model/plots/%name")
path_fun3b(name = "gof1.png", model = "run001")
#> [1] "archive/run001/plots/gof1.png"
# Returns: "archive/run001/plots/gof1.png"

# Format based on script name
path_fun3c <- pathFormat(path.format = "%script/%name_%model")
path_fun3c(name = "gof1.png", model = "run001", script = "myscript")
#> [1] "myscript/gof1_run001.png"
# Returns: "myscript/gof1_run001.png"

# Supplying default model/script to pathFormat() itself
path_fun3d <- pathFormat(path.format = "%script/%name_%model",
                         model = "run001", script = "myscript")
path_fun3d(name = "gof1.png")
#> [1] "myscript/gof1_run001.png"
# Returns: "myscript/gof1_run001.png"

# Explicit arguments to the generated function override the defaults
path_fun3d(name = "gof1.png", model = "run002")
#> [1] "myscript/gof1_run002.png"
# Returns: "myscript/gof1_run002.png"

# Custom preprocessing of script (and/or model) via path.funs
path_fun3e <- pathFormat(path.format = "%script/%name_%model",
                         path.funs = list(script = function(x) substr(basename(x), 1, 4)))
path_fun3e(name = "gof1.png", model = "run001", script = "myscript.R")
#> [1] "mysc/gof1_run001.png"
# Returns: "mysc/gof1_run001.png"

# Custom function (passed through unchanged)
my_fun <- function(name, model, subdir) {
  paste0("custom/", model, "/", name)
}
path_fun4 <- pathFormat(path.format = my_fun)
identical(path_fun4, my_fun)  # TRUE
#> [1] TRUE
```
