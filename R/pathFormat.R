##' Create a function to derive output file paths from a path format
##'
##' Generates a function that constructs file paths according to a path format
##' (a template with placeholders). This is particularly useful in modeling workflows where outputs
##' need to be organized by model name, with consistent naming conventions across files.
##'
##' @param path.format A character string specifying a path format template, or a
##'   custom function. If a function is provided, it is returned without modification.
##'
##'   Templates are specified using \code{\%model}, \code{\%script}, and \code{\%name}
##'   placeholders, separated by \code{/} to indicate directory levels. The last
##'   \code{/}-separated component is treated as the file name pattern; any earlier
##'   components are directory names. \code{\%model} is replaced by the model name
##'   (with any file extension stripped), \code{\%script} by the script name (with
##'   any file extension stripped), and \code{\%name} by \code{name} (also with its
##'   extension stripped, re-attached at the end). Any other characters (e.g.
##'   \code{-}, \code{_}) are kept as literal separators. Examples:
##'   \describe{
##'     \item{\code{"\%model/\%name_\%model"}}{Creates paths like \code{model_name/file_model_name.ext}.
##'           Example: \code{"run001/gof_run001.png"}.}
##'     \item{\code{"\%name_\%model"}}{Creates paths like \code{file_model_name.ext} with no
##'           model subdirectory. Example: \code{"gof_run001.png"}}
##'     \item{\code{"\%model/\%model-\%name"}}{Creates paths like \code{model_name/model_name-file.ext}.
##'           Example: \code{"run001/run001-gof.png"}}
##'     \item{\code{"\%model-\%name"}}{Creates paths like \code{model_name-file.ext} with no
##'           model subdirectory. Example: \code{"run001-gof.png"}}
##'     \item{\code{"\%script/\%name_\%model"}}{Creates paths like \code{script_name/file_model_name.ext}.
##'           Example: \code{"myscript/gof_run001.png"}}
##'   }
##'   Any other combination of \code{\%model}, \code{\%script}, \code{\%name}, literal text,
##'   and \code{/} is supported - e.g. \code{"archive/\%model/plots/\%name"}.
##'   Default is \code{"\%name"}, i.e. \code{name} is used as is (relative to \code{dir}).
##' @param dir Character string specifying the base directory for all outputs. This is
##'   prepended to all generated paths. Default is \code{NULL} (current directory).
##' @param model Default value for the \code{model} argument of the generated function.
##'   Used whenever the generated function is called without explicitly supplying
##'   \code{model}. Default is \code{NULL}.
##' @param script Default value for the \code{script} argument of the generated function.
##'   Used whenever the generated function is called without explicitly supplying
##'   \code{script}. Default is \code{NULL}.
##' @param path.funs Optional named list of functions used to preprocess
##'   \code{model} and/or \code{script} before they are substituted into
##'   \code{path.format}. Supported names are \code{model} and \code{script}. Each
##'   function takes the value as its single argument and returns a character
##'   string. Elements not supplied keep the default, which is
##'   \code{function(x) fnExtension(basename(x), "")} (strip directory and file
##'   extension). Note that a supplied function replaces the default entirely, so
##'   e.g. extension stripping must be included if still wanted. Example:
##'   \code{path.funs = list(script = function(x) substr(basename(x), 1, 4))}.
##'   Default is \code{NULL}.
##' @param structure Deprecated. Use \code{path.format}.
##'
##' @details
##' The returned function accepts the following arguments:
##' \describe{
##'   \item{\code{name}}{The base file name (typically without extension)}
##'   \item{\code{model}}{A model identifier (character string or model object). If a
##'         model object (list) is provided, the \code{$mod} element is extracted and
##'         used as the model name. The file extension is automatically removed from
##'         the model name (e.g., "model.lst" becomes "model").}
##'   \item{\code{script}}{A script identifier (character string, typically a script file
##'         name). The file extension is automatically removed (e.g., "myscript.R"
##'         becomes "myscript").}
##'   \item{\code{subdir}}{Optional subdirectory. Inserted between the directory
##'         components of \code{path.format} and the file name component.}
##'   \item{\code{dir}}{Base directory. Overrides the \code{dir} argument passed to
##'         \code{pathFormat}.}
##'   \item{\code{quiet}}{If \code{TRUE}, suppress messages about ignored
##'         \code{model}/\code{script} arguments. Default \code{FALSE}.}
##' }
##'
##' The generated function handles missing \code{model}/\code{script} arguments
##' gracefully - if \code{path.format} does not contain \code{\%model} (or
##' \code{\%script}), the corresponding argument is optional and ignored (with a
##' message if supplied anyway). If \code{path.format} does contain \code{\%model}
##' (or \code{\%script}), the corresponding argument is required.
##'
##' @return A function with signature \code{function(name, model, script, subdir, dir, quiet = FALSE)} that
##'   generates file paths according to the specified path format. If \code{path.format}
##'   is already a function, it is returned unchanged.
##'
##' @seealso
##' \code{\link{writer}} for using path formats in writing functions,
##' \code{\link{ggwrite}}, \code{\link{ftwrite}}, \code{\link{lwrite}} for functions
##' that accept \code{fun.path} arguments
##'
##' @import NMdata
##' @export
##'
##' @examples
##' # Create a path function (default format)
##' path_fun <- pathFormat(path.format = "%model/%name_%model")
##'
##' # Use it to generate paths
##' path_fun(name = "gof1.png", model = "run001")
##' # Returns: "run001/gof1_run001.png"
##'
##' path_fun(name = "gof1.png", model = "run001", subdir = "diagnostics")
##' # Returns: "run001/diagnostics/gof1_run001.png"
##'
##' # With base directory
##' path_fun2 <- pathFormat(path.format = "%model/%model-%name", dir = "outputs")
##' path_fun2(name = "gof1.png", model = "run001")
##' # Returns: "outputs/run001/run001-gof1.png"
##'
##' # Different format
##' path_fun3 <- pathFormat(path.format = "%model-%name")
##' path_fun3(name = "gof1.png", model = "run001")
##' # Returns: "run001-gof1.png"
##'
##' # Fully custom format
##' path_fun3b <- pathFormat(path.format = "archive/%model/plots/%name")
##' path_fun3b(name = "gof1.png", model = "run001")
##' # Returns: "archive/run001/plots/gof1.png"
##'
##' # Format based on script name
##' path_fun3c <- pathFormat(path.format = "%script/%name_%model")
##' path_fun3c(name = "gof1.png", model = "run001", script = "myscript")
##' # Returns: "myscript/gof1_run001.png"
##'
##' # Supplying default model/script to pathFormat() itself
##' path_fun3d <- pathFormat(path.format = "%script/%name_%model",
##'                          model = "run001", script = "myscript")
##' path_fun3d(name = "gof1.png")
##' # Returns: "myscript/gof1_run001.png"
##'
##' # Explicit arguments to the generated function override the defaults
##' path_fun3d(name = "gof1.png", model = "run002")
##' # Returns: "myscript/gof1_run002.png"
##'
##' # Custom preprocessing of script (and/or model) via path.funs
##' path_fun3e <- pathFormat(path.format = "%script/%name_%model",
##'                          path.funs = list(script = function(x) substr(basename(x), 1, 4)))
##' path_fun3e(name = "gof1.png", model = "run001", script = "myscript.R")
##' # Returns: "mysc/gof1_run001.png"
##'
##' # Custom function (passed through unchanged)
##' my_fun <- function(name, model, subdir) {
##'   paste0("custom/", model, "/", name)
##' }
##' path_fun4 <- pathFormat(path.format = my_fun)
##' identical(path_fun4, my_fun)  # TRUE
pathFormat <- function(path.format="%name",dir,model=NULL,script=NULL,path.funs=NULL,structure){

  ## structure is deprecated in favor of path.format
  if(!missing(structure)){
    if(!missing(path.format)) stop("Use only one of path.format and structure. structure is deprecated.")
    message("The structure argument is deprecated. Use path.format.")
    path.format <- structure
  }

  if(missing(path.format)) path.format <- NULL
  if(is.null(path.format)) path.format <- "%name"
  if(is.function(path.format)) return(path.format)
  ## internal name kept for the template string below
  fmt <- path.format

  if(missing(dir)) dir <- NULL
  default.model <- model
  default.script <- script
  default.dir <- dir

  ## Preprocessing functions applied to model/script before substitution.
  ## User-supplied functions in path.funs replace the defaults.
  funs.default <- list(
    model  = function(x) fnExtension(basename(x),""),
    script = function(x) fnExtension(basename(x),"")
  )
  if(!is.null(path.funs)){
    if(!is.list(path.funs) || is.null(names(path.funs)) || any(names(path.funs)=="")){
      stop("path.funs must be a named list of functions.")
    }
    unsupported.funs <- setdiff(names(path.funs), names(funs.default))
    if(length(unsupported.funs)){
      stop("Unsupported element(s) in path.funs: ",
           paste(unsupported.funs, collapse=", "),
           ". Supported elements are model and script.")
    }
    if(!all(vapply(path.funs, is.function, logical(1)))){
      stop("All elements of path.funs must be functions.")
    }
    funs.default[names(path.funs)] <- path.funs
  }
  fun.model  <- funs.default$model
  fun.script <- funs.default$script

  filePathSimple <- NMdata:::filePathSimple

  ## Backward compatibility with the old named structures
  fmt <- switch(fmt,
                "model/file_model" = "%model/%name_%model",
                "file_model"       = "%name_%model",
                "model/model-file" = "%model/%model-%name",
                "model-file"       = "%model-%name",
                fmt)

  if(!grepl("%name", fmt, fixed=TRUE)){
    stop("path.format must contain a %name placeholder.")
  }

  ## Check for unsupported %placeholders (e.g. typos like %scrpt). Only
  ## %model, %script, and %name are supported. Literal text (including a
  ## literal "%" not followed by a known placeholder word) is left untouched.
  placeholders <- regmatches(fmt, gregexpr("%[[:alpha:]]+", fmt))[[1]]
  unsupported <- setdiff(placeholders, c("%model","%script","%name"))
  if(length(unsupported)){
    stop("Unsupported placeholder(s) in path.format: ",
         paste(unsupported, collapse=", "),
         ". Supported placeholders are %model, %script, and %name.")
  }

  ## Split the template into directory components and the file name
  ## component (the part after the last "/").
  parts <- strsplit(fmt, "/", fixed=TRUE)[[1]]
  dir.parts  <- if(length(parts)>1) parts[-length(parts)] else character(0)
  file.part  <- parts[length(parts)]

  ## Substitutes %model, %script (and drops placeholder-only dir levels when
  ## the corresponding value is NULL) and %name in a single template component.
  substPart <- function(part, model.name, script.name, file.name){
    if(grepl("%model", part, fixed=TRUE)){
      if(is.null(model.name)){
        if(part=="%model") return(NULL) # whole component was just %model
        part <- gsub("%model", "", part, fixed=TRUE)
      } else {
        part <- gsub("%model", model.name, part, fixed=TRUE)
      }
    }
    if(grepl("%script", part, fixed=TRUE)){
      if(is.null(script.name)){
        if(part=="%script") return(NULL) # whole component was just %script
        part <- gsub("%script", "", part, fixed=TRUE)
      } else {
        part <- gsub("%script", script.name, part, fixed=TRUE)
      }
    }
    part <- gsub("%name", file.name, part, fixed=TRUE)
    part
  }

  needs.model  <- grepl("%model", fmt, fixed=TRUE)
  needs.script <- grepl("%script", fmt, fixed=TRUE)

  ## NULL is treated like a missing argument so that callers (e.g. lwrite())
  ## passing model=NULL/script=NULL/dir=NULL still get the pathFormat() defaults.
  fun.path <- function(name,model,script,subdir,dir,quiet=FALSE){
    if(missing(dir) || is.null(dir)) dir <- default.dir
    if(missing(model) || is.null(model)) model <- default.model

    if(is.null(model)){
      if(needs.model) stop("path.format contains %model, so model must be provided.")
      model <- NULL
    } else if(!needs.model && !is.null(model)){
      if(!quiet) message("path.format does not contain %model; the model argument is ignored in file path.")
    }
    model.name <- NULL
    if(!is.null(model)){
      if(is.list(model)) model <- model$mod
      model.name <- fun.model(model)
    }

    if(missing(script) || is.null(script)) script <- default.script

    if(is.null(script)){
      if(needs.script) stop("path.format contains %script, so script must be provided.")
      script <- NULL
    } else if(!needs.script && !is.null(script)){
      if(!quiet) message("path.format does not contain %script; the script argument is ignored in file path.")
    }
    script.name <- NULL
    if(!is.null(script)){
      script.name <- fun.script(script)
    }

    ## file.part may itself contain an extension-carrying %file; keep the
    ## extension of `name` attached at the very end.
    ext <- fnExtension(name)
    base.name <- fnExtension(name,"")

    dir.resolved <- vapply(dir.parts, substPart, character(1),
                            model.name=model.name, script.name=script.name,
                            file.name=base.name)
    dir.resolved <- dir.resolved[!is.na(dir.resolved) & nzchar(dir.resolved)]

    file.resolved <- substPart(file.part, model.name=model.name,
                                script.name=script.name, file.name=base.name)
    if(nzchar(ext)) file.resolved <- fnExtension(file.resolved, ext)

    if(missing(subdir)) subdir <- NULL

    do.call(filePathSimple, c(list(dir), as.list(dir.resolved), list(subdir, file.resolved)))
  }

  fun.path
}

##' Deprecated: use pathFormat()
##'
##' \code{pathStruct()} is deprecated. Use \code{\link{pathFormat}}.
##' @param ... Passed to \code{\link{pathFormat}}.
##' @return See \code{\link{pathFormat}}.
##' @keywords internal
##' @export
pathStruct <- function(...){
  message("pathStruct() is deprecated. Use pathFormat().")
  pathFormat(...)
}

##' Resolve path.format (and deprecated fun.path) to a path function
##'
##' Internal helper used by \code{\link{ggwrite}}, \code{\link{ftwrite}},
##' \code{\link{datwrite}}, and \code{\link{writer}}.
##'
##' @param path.format Character string, function or \code{NULL}.
##' @param fun.path Deprecated alias for \code{path.format}.
##' @return A path function as returned by \code{\link{pathFormat}}, or
##'   \code{NULL} if neither argument is given.
##' @keywords internal
argPathFormat <- function(path.format=NULL, fun.path=NULL){
  if(!is.null(fun.path)){
    if(!is.null(path.format)) stop("Use only one of path.format and fun.path. fun.path is deprecated.")
    message("The fun.path argument is deprecated. Use path.format.")
    path.format <- fun.path
  }
  if(is.null(path.format)) return(NULL)
  pathFormat(path.format=path.format)
}

##' Call a path function safely, forwarding script only if supported
##'
##' Internal helper used by \code{\link{ggwrite}}, \code{\link{ftwrite}},
##' \code{\link{datwrite}}, and \code{\link{lwrite}} to call a \code{fun.path}
##' function (as returned by \code{\link{pathFormat}}, or user-supplied) with
##' \code{name}, \code{model}, \code{subdir}, and (if supported) \code{script}.
##' This avoids "unused argument" errors when a custom \code{fun.path} function
##' does not accept a \code{script} argument.
##'
##' @param fun.path A function as returned by \code{\link{pathFormat}}, or any
##'   custom function accepting at least \code{name}, \code{model}, and
##'   \code{subdir} arguments.
##' @param name Character string, file name passed to \code{fun.path}.
##' @param model Model identifier passed to \code{fun.path}.
##' @param script Script identifier. Only forwarded to \code{fun.path} if it has
##'   a \code{script} formal argument.
##' @param subdir Subdirectory passed to \code{fun.path}.
##' @param quiet Logical. If \code{TRUE}, suppresses messages about ignored
##'   \code{model}/\code{script} arguments. Only forwarded to \code{fun.path} if
##'   it has a \code{quiet} formal argument.
##'
##' @return The result of calling \code{fun.path} with the applicable arguments.
##'
##' @keywords internal
applyFunPath <- function(fun.path, name, model=NULL, script=NULL, subdir=NULL, quiet=FALSE){
  path.args <- list(name=name, model=model, subdir=subdir)
  if("script"%in%names(formals(fun.path))) path.args$script <- script
  if("quiet"%in%names(formals(fun.path))) path.args$quiet <- quiet
  do.call(fun.path, path.args)
}

