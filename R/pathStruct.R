##' Create a function to derive structured output file paths
##'
##' Generates a function that constructs file paths according to a specified directory
##' structure pattern. This is particularly useful in modeling workflows where outputs
##' need to be organized by model name, with consistent naming conventions across files.
##'
##' @param structure A character string specifying a predefined path structure, or a
##'   custom function. If a function is provided, it is returned without modification.
##'   Predefined structures include:
##'   \describe{
##'     \item{\code{"model/file_model"}}{Creates paths like \code{model_name/file_model_name.ext}.
##'           Example: \code{"run001/gof_run001.png"}}
##'     \item{\code{"file_model"}}{Creates paths like \code{file_model_name.ext} with no
##'           model subdirectory. Example: \code{"gof_run001.png"}}
##'     \item{\code{"model/model-file"}}{Creates paths like \code{model_name/model_name-file.ext}.
##'           Example: \code{"run001/run001-gof.png"}}
##'     \item{\code{"model-file"}}{Creates paths like \code{model_name-file.ext} with no
##'           model subdirectory. Example: \code{"run001-gof.png"}}
##'   }
##'   Default is \code{"model/file_model"}.
##' @param subdir Character string specifying an additional subdirectory to include in
##'   the path structure. This is applied within the model directory if applicable.
##'   Default is \code{NULL} (no subdirectory).
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
##'         passed to \code{pathStruct})}
##' }
##'
##' The generated function handles \code{NULL} model arguments gracefully - if
##' \code{model = NULL}, the model name component is omitted from the path.
##'
##' Path components are combined using \code{\link{filePathSimple}}, which handles
##' platform-specific path separators and normalizes the resulting path.
##'
##' @section Structure Details:
##' \describe{
##'   \item{\code{"model/file_model"}}{
##'     Organizes by model with file names appended with model name.
##'     Good for: Multiple files per model, easy browsing by model.
##'     Path: \code{dir/model_name/subdir/file_model_name.ext}
##'   }
##'   \item{\code{"file_model"}}{
##'     Flat structure with model name in file name only.
##'     Good for: Fewer files, simple directory structure.
##'     Path: \code{dir/subdir/file_model_name.ext}
##'   }
##'   \item{\code{"model/model-file"}}{
##'     Organizes by model with hyphen separator in file names.
##'     Good for: Consistent model prefix in all file names.
##'     Path: \code{dir/model_name/subdir/model_name-file.ext}
##'   }
##'   \item{\code{"model-file"}}{
##'     Flat structure with hyphen-separated model and file name.
##'     Good for: Simple structure with clear model-file association.
##'     Path: \code{dir/subdir/model_name-file.ext}
##'   }
##' }
##'
##' @return A function with signature \code{function(name, model, subdir)} that
##'   generates file paths according to the specified structure. If \code{structure}
##'   is already a function, it is returned unchanged.
##'
##' @seealso
##' \code{\link{filePathSimple}} for path combination,
##' \code{\link{writer}} for using path structures in writing functions,
##' \code{\link{ggwrite}}, \code{\link{ftwrite}}, \code{\link{lwrite}} for functions
##' that accept \code{fun.path} arguments
##'
##' @export
##'
##' @examples
##' # Create a path structure function
##' path_fun <- pathStruct(structure = "model/file_model")
##'
##' # Use it to generate paths
##' path_fun(name = "gof1.png", model = "run001")
##' # Returns: "run001/gof1_run001.png"
##'
##' path_fun(name = "gof1.png", model = "run001", subdir = "diagnostics")
##' # Returns: "run001/diagnostics/gof1_run001.png"
##'
##' # With base directory
##' path_fun2 <- pathStruct(structure = "model/model-file", dir = "outputs")
##' path_fun2(name = "gof1.png", model = "run001")
##' # Returns: "outputs/run001/run001-gof1.png"
##'
##' # Different structure
##' path_fun3 <- pathStruct(structure = "model-file")
##' path_fun3(name = "gof1.png", model = "run001")
##' # Returns: "run001-gof1.png"
##'
##' # Handle NULL model
##' path_fun(name = "gof1.png", model = NULL)
##' # Returns: "gof1.png"
##'
##' # Custom function (passed through unchanged)
##' my_fun <- function(name, model, subdir) {
##'   paste0("custom/", model, "/", name)
##' }
##' path_fun4 <- pathStruct(structure = my_fun)
##' identical(path_fun4, my_fun)  # TRUE
pathStruct <- function(structure="model/file_model",dir){

    if(missing(structure)) structure <- NULL
    if(is.null(structure)) structure <- "model/file_model"
    if(is.function(structure)) return(structure)

    if(missing(dir)) dir <- NULL

    fun.path <- NULL
    if(structure=="model/file_model"){
        fun.path <- function(name,model,subdir){
            if(is.null(model)) return(filePathSimple(dir,name))
            if(is.list(model)) {
                model <- model$mod 
            }
            model.name <- basename(model) |> fnExtension("")
            if(missing(subdir)) subdir <- NULL
            
            filePathSimple(dir,
                           model.name,
                           subdir,
                           fnAppend(name,model.name,allow.noext=TRUE)
                           )
        }
    }

    if(structure=="file_model"){
        fun.path <- function(name,model,subdir){
            if(is.null(model)) return(filePathSimple(dir,name))
            if(is.list(model)) {
                model <- model$mod 
            }
            model.name <- basename(model) |> fnExtension("")
            if(missing(subdir)) subdir <- NULL
            
            filePathSimple(dir,
                           subdir,
                           fnAppend(name,model.name,allow.noext=TRUE)
                           )
        }
    }

    ## "gof1.png", model="103", > "103/103-gof1.png"
    ## name="gof1", model="103", > "103/103-gof1.png"
    if(structure=="model/model-file"){
        fun.path <- function(name,model,subdir){
            if(is.null(model)) filePathSimple(dir,name,subdir)
            if(is.list(model)) {
                model <- model$mod 
            }
            ## model.lst -> model
            model.name <- basename(model) |> fnExtension("")
            if(missing(subdir)) subdir <- NULL

            filePathSimple(dir,
                           model.name,
                           subdir,
                           paste(model.name,name,sep="-")
                           )
        }
    }

    if(structure=="model-file"){
        fun.path <- function(name,model,subdir){
            if(is.null(model)) filePathSimple(dir,name)
            if(is.list(model)) {
                model <- model$mod 
            }
            model.name <- basename(model) |> fnExtension("")
            if(missing(subdir)) subdir <- NULL
            
            filePathSimple(dir,
                           subdir,
                           paste(model.name,name,sep="-")
                           )
        }
    }

    if(is.null(fun.path)){
        stop("structure not recognized.")
    }


    fun.path
}
