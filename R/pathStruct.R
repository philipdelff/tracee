##' Create a function to derive structured output file paths
##'
##' Generates a function that constructs file paths according to a specified directory
##' structure pattern. This is particularly useful in modeling workflows where outputs
##' need to be organized by model name, with consistent naming conventions across files.
##'
##' @param structure A character string specifying a path structure template, or a
##'   custom function. If a function is provided, it is returned without modification.
##'
##'   Templates are specified using \code{\%model} and \code{\%file} placeholders,
##'   separated by \code{/} to indicate directory levels. The last \code{/}-separated
##'   component is treated as the file name pattern; any earlier components are
##'   directory names. \code{\%model} is replaced by the model name (with any file
##'   extension stripped), and \code{\%file} by \code{name} (also with its extension
##'   stripped, re-attached at the end). Any other characters (e.g. \code{-}, \code{_})
##'   are kept as literal separators. Examples:
##'   \describe{
##'     \item{\code{"\%model/\%file_\%model"}}{Creates paths like \code{model_name/file_model_name.ext}.
##'           Example: \code{"run001/gof_run001.png"}. This is the default.}
##'     \item{\code{"\%file_\%model"}}{Creates paths like \code{file_model_name.ext} with no
##'           model subdirectory. Example: \code{"gof_run001.png"}}
##'     \item{\code{"\%model/\%model-\%file"}}{Creates paths like \code{model_name/model_name-file.ext}.
##'           Example: \code{"run001/run001-gof.png"}}
##'     \item{\code{"\%model-\%file"}}{Creates paths like \code{model_name-file.ext} with no
##'           model subdirectory. Example: \code{"run001-gof.png"}}
##'   }
##'   Any other combination of \code{\%model}, \code{\%file}, literal text, and \code{/}
##'   is supported - e.g. \code{"archive/\%model/plots/\%file"}.
##'   Default is \code{"\%model/\%file_\%model"}.
##' @param dir Character string specifying the base directory for all outputs. This is
##'   prepended to all generated paths. Default is \code{NULL} (current directory).
##'
##' @details
##' The returned function accepts three arguments:
##' \describe{
##'   \item{\code{name}}{The base file name (typically without extension)}
##'   \item{\code{model}}{A model identifier (character string or model object). If a
##'         model object (list) is provided, the \code{$mod} element is extracted and
##'         used as the model name. The file extension is automatically removed from
##'         the model name (e.g., "model.lst" becomes "model").}
##'   \item{\code{subdir}}{Optional subdirectory (overrides the \code{subdir} argument
##'         passed to \code{pathStruct}). Inserted between the directory components of
##'         \code{structure} and the file name component.}
##' }
##'
##' The generated function handles \code{NULL} model arguments gracefully - if
##' \code{model = NULL}, the \code{\%model} placeholder (and any literal text
##' immediately bound to it, including an entire directory level consisting of
##' \code{\%model} alone) is dropped from the path.
##'
##' @return A function with signature \code{function(name, model, subdir)} that
##'   generates file paths according to the specified structure. If \code{structure}
##'   is already a function, it is returned unchanged.
##'
##' @seealso
##' \code{\link{writer}} for using path structures in writing functions,
##' \code{\link{ggwrite}}, \code{\link{ftwrite}}, \code{\link{lwrite}} for functions
##' that accept \code{fun.path} arguments
##'
##' @import NMdata
##' @export
##'
##' @examples
##' # Create a path structure function (default template)
##' path_fun <- pathStruct(structure = "%model/%file_%model")
##'
##' # Use it to generate paths
##' path_fun(name = "gof1.png", model = "run001")
##' # Returns: "run001/gof1_run001.png"
##'
##' path_fun(name = "gof1.png", model = "run001", subdir = "diagnostics")
##' # Returns: "run001/diagnostics/gof1_run001.png"
##'
##' # Handle NULL model (error because structure contains model)
##' \dontrun{
##' path_fun(name = "gof1.png", model = NULL)
##' }
##'
##' # With base directory
##' path_fun2 <- pathStruct(structure = "%model/%model-%file", dir = "outputs")
##' path_fun2(name = "gof1.png", model = "run001")
##' # Returns: "outputs/run001/run001-gof1.png"
##'
##' # Different structure
##' path_fun3 <- pathStruct(structure = "%model-%file")
##' path_fun3(name = "gof1.png", model = "run001")
##' # Returns: "run001-gof1.png"
##'
##' # Fully custom structure
##' path_fun3b <- pathStruct(structure = "archive/%model/plots/%file")
##' path_fun3b(name = "gof1.png", model = "run001")
##' # Returns: "archive/run001/plots/gof1.png"
##'
##' # Custom function (passed through unchanged)
##' my_fun <- function(name, model, subdir) {
##'   paste0("custom/", model, "/", name)
##' }
##' path_fun4 <- pathStruct(structure = my_fun)
##' identical(path_fun4, my_fun)  # TRUE
pathStruct <- function(structure="%model/%file_%model",dir){

  
  if(missing(structure)) structure <- NULL
  if(is.null(structure)) structure <- "%model/%file_%model"
  if(is.function(structure)) return(structure)

  if(missing(dir)) dir <- NULL

  filePathSimple <- NMdata:::filePathSimple

  ## Backward compatibility with the old named structures
  structure <- switch(structure,
                       "model/file_model" = "%model/%file_%model",
                       "file_model"       = "%file_%model",
                       "model/model-file" = "%model/%model-%file",
                       "model-file"       = "%model-%file",
                       structure)

  if(!grepl("%file", structure, fixed=TRUE)){
    stop("structure must contain a %file placeholder.")
  }

  ## Split the template into directory components and the file name
  ## component (the part after the last "/").
  parts <- strsplit(structure, "/", fixed=TRUE)[[1]]
  dir.parts  <- if(length(parts)>1) parts[-length(parts)] else character(0)
  file.part  <- parts[length(parts)]

  ## Substitutes %model (and drops %model-only dir levels when model is NULL)
  ## and %file in a single template component.
  substPart <- function(part, model.name, file.name){
    if(grepl("%model", part, fixed=TRUE)){
      if(is.null(model.name)){
        if(part=="%model") return(NULL) # whole component was just %model
        part <- gsub("%model", "", part, fixed=TRUE)
      } else {
        part <- gsub("%model", model.name, part, fixed=TRUE)
      }
    }
    part <- gsub("%file", file.name, part, fixed=TRUE)
    part
  }

  needs.model <- grepl("%model", structure, fixed=TRUE)

  fun.path <- function(name,model,subdir){
    if(missing(model)) model <- NULL

    if(is.null(model)){
      if(needs.model) stop("structure contains %model, so model must be provided.")
      model <- NULL
    } else if(!needs.model && !is.null(model)){
      message("structure does not contain %model; the model argument is ignored.")
    }
    model.name <- NULL
    if(!is.null(model)){
      if(is.list(model)) model <- model$mod
      model.name <- fnExtension(basename(model),"")
    }

    ## file.part may itself contain an extension-carrying %file; keep the
    ## extension of `name` attached at the very end.
    ext <- fnExtension(name)
    base.name <- fnExtension(name,"")

    dir.resolved <- vapply(dir.parts, substPart, character(1),
                            model.name=model.name, file.name=base.name)
    dir.resolved <- dir.resolved[!is.na(dir.resolved) & nzchar(dir.resolved)]

    file.resolved <- substPart(file.part, model.name=model.name, file.name=base.name)
    if(nzchar(ext)) file.resolved <- fnExtension(file.resolved, ext)

    if(missing(subdir)) subdir <- NULL

    do.call(filePathSimple, c(list(dir), as.list(dir.resolved), list(subdir, file.resolved)))
  }

  fun.path
}

