##' Write lists of outputs with flexible organization and naming
##'
##' Writes nested lists containing plots, tables, and data frames to files with
##' intelligent handling of list structure, automatic naming, and optional subdirectory
##' organization. This is particularly useful for organizing multiple related outputs
##' from a single analysis or model run.
##'
##' @param list A list (potentially nested) containing output objects: \code{ggplot}
##'   plots, \code{flextable} tables, and/or \code{data.frame}s. The list can be
##'   arbitrarily nested, and names at each level will be used to construct output
##'   file names.
##' @param model Character string or model object specifying the model name/identifier.
##'   Used for organizing output directories and stamping. If a list (model object)
##'   is provided, the \code{$label} element is extracted.
##' @param dir Character string specifying the base output directory. Default is
##'   \code{"."} (current directory).
##' @param path.format Character string or function defining the output path format.
##'   Passed to \code{\link{pathFormat}} to create a path-generating function.
##'   Examples: \code{"\%model/\%name_\%model"} (default), \code{"\%name_\%model"},
##'   \code{"\%script/\%name_\%model"}. See \code{\link{pathFormat}} for details.
##' @param structure Deprecated. Use \code{path.format}.
##' @param lists.as.subdirs Logical. If \code{TRUE}, nested lists create subdirectories
##'   rather than being incorporated into file names. Default is \code{FALSE}.
##'   \itemize{
##'     \item \code{FALSE}: nested list names become part of file names
##'           (e.g., \code{list1_item1.png})
##'     \item \code{TRUE}: nested lists create subdirectories
##'           (e.g., \code{list1/item1.png})
##'   }
##' @param name.all.pdf Character string specifying the filename to use when writing
##'   all plots to a single PDF file. Only used when the file name is empty and
##'   plots are being combined into one PDF. Default is \code{"main_all.pdf"}.
##' @param ... Additional arguments passed to \code{\link{writer}} and subsequently
##'   to the specific writer functions (\code{\link{ggwrite}}, \code{\link{ftwrite}},
##'   or \code{\link{datwrite}}). Common arguments include:
##'   \itemize{
##'     \item \code{script}: Path to script file for stamping
##'     \item \code{time}: Timestamp for stamping
##'     \item \code{formats.gg}: File formats for plots (default "png")
##'     \item \code{formats.ft}: File formats for tables (default "png")
##'     \item \code{formats.data}: File formats for data frames (default "rds")
##'     \item \code{canvas}: Canvas size for plots
##'     \item \code{quiet}: Suppress messages
##'     \item \code{stamp.format}, \code{stamp.funs}: Stamp text format, see \code{\link{createStamp}}
##'     \item \code{stamp.gg.style}, \code{stamp.ft.style}: Stamp styling for plots and tables, see \code{\link{writer}}
##'   }
##'
##' @details
##' The function processes lists recursively with the following behavior:
##' \enumerate{
##'   \item Separates plot objects from non-plot objects
##'   \item Combines plots into a single multi-page or multi-file output (see \code{\link{ggwrite}})
##'   \item Writes each non-plot object individually
##'   \item Handles nested lists according to \code{lists.as.subdirs}
##' }
##'
##' @section List Structure Handling:
##' The function intelligently flattens nested lists while preserving meaningful names:
##' \itemize{
##'   \item Named list elements use their names in output file names
##'   \item Plots in the same list are combined (behavior depends on \code{\link{ggwrite}} settings)
##'   \item Non-plot objects (tables, data frames) are written individually
##'   \item Nesting deeper than 2 levels will cause an error
##' }
##'
##' @section File Naming:
##' Output file names are constructed from:
##' \itemize{
##'   \item The name at each level of the list hierarchy
##'   \item The \code{model} identifier (if provided and if \code{path.format} uses it)
##'   \item Automatic numbering for unnamed elements
##' }
##'
##' For example, with \code{lists.as.subdirs = FALSE}:
##' \preformatted{
##' list(
##'   section1 = list(
##'     plot1 = p1,
##'     table1 = ft1
##'   )
##' )
##' }
##' produces: \code{section1_plot1.png}, \code{section1_table1.png}
##'
##' With \code{lists.as.subdirs = TRUE}, the same structure produces:
##' \code{section1/plot1.png}, \code{section1/table1.png}
##'
##' @section Plot Handling:
##' All \code{ggplot} objects within the same list level are collected and written
##' together using \code{\link{ggwrite}}. This allows creating multi-page PDFs or
##' sequentially numbered PNG files from a list of plots.
##'
##' @return Invisibly returns a list of results from the \code{\link{writer}} calls.
##'   The structure mirrors the input list structure.
##'
##' @seealso
##' \code{\link{writer}} for the underlying writing function,
##' \code{\link{ggwrite}} for plot-specific options,
##' \code{\link{ftwrite}} for table-specific options,
##' \code{\link{datwrite}} for data frame writing,
##' \code{\link{pathFormat}} for path format options,
##' \code{\link{is.listnotplot}} for list detection logic
##'
##' @export
##'
##' @examples
##' \dontrun{
##' library(ggplot2)
##' library(flextable)
##'
##' # Create some outputs
##' p1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
##' p2 <- ggplot(mtcars, aes(hp, mpg)) + geom_point()
##' ft1 <- flextable(head(mtcars))
##' df1 <- mtcars
##'
##' # Simple list
##' outputs <- list(
##'   scatter_wt = p1,
##'   scatter_hp = p2,
##'   summary_table = ft1,
##'   data = df1
##' )
##'
##' lwrite(outputs, model = "run001", dir = "outputs",script="myscript")
##' # Creates in outputs/run001/:
##' #   scatter_wt_run001.png
##' #   scatter_hp_run001.png
##' #   summary_table_run001.png
##' #   data_run001.rds
##' 
##' # Nested list with subdirectories
##' nested_outputs <- list(
##'   diagnostics = list(
##'     gof1 = p1,
##'     gof2 = p2
##'   ),
##'   tables = list(
##'     summary = ft1
##'   )
##' )
##'
##' lwrite(nested_outputs,
##'        model = "run001",
##'        dir = "outputs",
##'        lists.as.subdirs = TRUE)
##' # Creates:
##' #   outputs/run001/diagnostics/gof1_run001.png
##' #   outputs/run001/diagnostics/gof2_run001.png
##' #   outputs/run001/tables/summary_run001.png
##'
##' # With stamping
##' lwrite(outputs,
##'        model = "run001",
##'        dir = "outputs",
##'        script = "analysis.R",
##'        time = Sys.time())
##' }
##'
##' @import NMdata
##' @importFrom stats setNames



#### send name.main.pdf as name.pdf.onefile?

lwrite <- function(list,model,dir=".",path.format,lists.as.subdirs=FALSE,name.all.pdf="main_all.pdf",structure,...){

  ## structure is deprecated in favor of path.format
  if(!missing(structure)){
    if(!missing(path.format)) stop("Use only one of path.format and structure. structure is deprecated.")
    message("The structure argument is deprecated. Use path.format.")
    path.format <- structure
  }

  filePathSimple <- NMdata:::filePathSimple
  
  name.main <- "name.root.elements.random.qqq"

  ## subdir is disabled because it doesn't make much sense with the new flexible path.format syntax
  subdir <- NULL

  ## lwrite() intentionally uses a single fun.path/path.format for all elements
  ## (plots, tables, data frames) of the list - some of these element-level
  ## writer calls may not need %model/%script even though the user-level
  ## path.format was designed with other element types in mind. Repeating the
  ## "path.format does not contain %model/%script; ... ignored" message once per
  ## file is just noise here, so always resolve paths quietly at this level,
  ## regardless of the "quiet" argument (which still controls "file written"
  ## style messages from the underlying ggwrite()/ftwrite()/datwrite() calls).
  quietFunPath <- function(fun.path){
    force(fun.path)
    function(name, model=NULL, script=NULL, subdir=NULL, quiet=FALSE, ...){
      fun.path(name=name, model=model, script=script, subdir=subdir, quiet=TRUE, ...)
    }
  }
  
  if(missing(model)) model <- NULL
  
  if(is.list(model)) {
    model <- model$label
  }

  dots.outer <- list(...)
  script.outer <- dots.outer$script
  
  if(missing(path.format)) path.format <- NULL

  if(is.function(path.format)){
    ## If the function accepts dir (as pathFormat()-generated functions do),
    ## a dir supplied to lwrite() is passed on and overrides its default.
    ## Otherwise dir is prepended to the returned path.
    fun.struct <- path.format
    if(!missing(dir) && !is.null(dir)){
      dir.lwrite <- dir
      if("dir"%in%names(formals(path.format))){
        fun.struct <- function(name, ...) path.format(name=name, dir=dir.lwrite, ...)
      } else {
        fun.struct <- function(name, ...){
          filePathSimple(dir.lwrite, path.format(name=name, ...))
        }
      }
    }
  } else {
    fun.struct <- pathFormat(path.format=path.format,dir=dir)
  }

  fun.path <- quietFunPath(fun.struct)

  dir.model <-
    dirname(applyFunPath(fun.path,name="dummy",model=model,script=script.outer,subdir=subdir,quiet=TRUE))

  OutputDirCreate(dir.model)

  ## make sure list2 is a list of lists
  nolist <- list[!sapply(list,is.listnotplot)]
  list2 <- c(list[sapply(list,is.listnotplot)],
             setNames(list(nolist),name.main)
             )
  list2 <- list2[sapply(list2,function(x)length(x) > 0)]
  
  lwrite1 <- function(x,name,model,subdir,...){
    
    if(any(
      sapply(x,is.listnotplot)
    )) stop("too deep listing")
    
    ## Collect plots in one list. When should this be done?
    plots <- x[sapply(x,is.gg)]
    notplots <- x[!sapply(x,is.gg)]
    
    res <- list()

    ## Preserve the subdir passed in to this call (e.g. the top-level subdir
    ## argument to lwrite()) so it is not silently discarded below.
    subdir.in <- subdir
    
    if(length(plots)) {

      subdir <- subdir.in
      ## The list name must not be duplicated between subdir and file name.
      ## If lists.as.subdirs is TRUE, the name is carried entirely by subdir,
      ## so file must not also include it (previously only name==name.main
      ## was handled here, causing "list1/list1_scatter_wt...").
      file.plots <- name
      if(lists.as.subdirs) {
        subdir <- filePathSimple(subdir,name)
        file.plots <- ""
      }
      if(name==name.main) file.plots <- ""
      
      res1 <- writer(x=plots,
                     file=file.plots,
                     model=model,
                     path.format=fun.path,
                 subdir=subdir,
                   name.all.pdf=  name.all.pdf,
                     ...)
      res <- append(res,res1)
    }
    
    ## dots <- list(...)

    if(length(notplots)) {
      subdir <- subdir.in
      names.notplots <- names(notplots)
      if(name==name.main) name <- ""
      if(lists.as.subdirs) {
        subdir <- filePathSimple(subdir,name)
      } else if(nzchar(name)) {
        ## Build names element-wise. A vectorized fnAppend() call did not
        ## preserve the order of names.notplots, so objects were written
        ## under each other's names (e.g. the flextable as "data").
        names(notplots) <- vapply(names.notplots,
                                  function(nm) fnAppend(name, nm, allow.noext=TRUE),
                                  character(1), USE.NAMES=FALSE)
      }
      ## If name is "" (top-level elements) the list names are used as they are.

      res1 <- mapply(FUN=writer,
                     notplots,
                     ## file=fnAppend(file.out,names(notplots),allow.noext=TRUE),
                     ## file=fnAppend(name,names(notplots),allow.noext = TRUE),
                     ## file = sapply(names(notplots),filePathSimple(dir,subdir,x)),
                     file = names(notplots),
                     ## file=name,

                     ## name=names(notplots),
                     MoreArgs=list(
                       model=model,
                       path.format=fun.path,
                       subdir=subdir,
                       ...))
      res <- append(res,res1)
    }

    return(invisible(res))
  }

  
   
  if(lists.as.subdirs){
    
    res <- mapply(lwrite1,list2,name=names(list2),
                  MoreArgs=list(model=model,
                                lists.as.subdirs=lists.as.subdirs,
                                subdir=subdir,
                                ...),
                  SIMPLIFY=FALSE)
  } else {  

  res <- mapply(lwrite1,list2,name=names(list2),
                  MoreArgs=list(model=model,
                                lists.as.subdirs=lists.as.subdirs,
                                subdir=subdir,
                                ...),
                  SIMPLIFY=FALSE)
  }


  invisible(res)

}
