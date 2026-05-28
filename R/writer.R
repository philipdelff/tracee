##' Write various R objects to files with flexible formatting
##'
##' A unified interface for ggwrite, ftwrite, and NMwriteData that automatically
##' detects object type and applies the appropriate writing function with
##' consistent file naming and stamping conventions.
##'
##' @param x Object to write. Can be a ggplot, flextable, data.frame, or list of such objects.
##' @param file Character string specifying the base file name (without extension).
##' @param formats.ft Character vector of file formats for flextable objects. 
##'   Default is "png". See \code{\link{ftwrite}} for supported formats.
##' @param formats.gg Character vector of file formats for ggplot objects. 
##'   Default is "png". See \code{\link{ggwrite}} for supported formats.
##' @param formats.data Character vector of file formats for data frame objects. 
##'   Default is "rds". Passed to NMwriteData() as format.write if file is a data set.
##' @param fun.path Function or character string defining the directory structure. 
##'   See \code{\link{pathStruct}} for details on available structures.
##' @param subdir Character string specifying a subdirectory within the path structure.
##' @param ... Additional arguments passed to the specific writer functions 
##'   (\code{\link{ggwrite}}, \code{\link{ftwrite}}, or \code{\link{NMwriteData}}).
##'   Common arguments include \code{script}, \code{time}, \code{model}, \code{save}, 
##'   \code{show}, \code{canvas}, and \code{quiet}.
##'
##' @details
##' The function dispatches to appropriate writer functions based on object class:
##' \itemize{
##'   \item flextable objects -> \code{\link{ftwrite}}
##'   \item data.frame objects -> \code{\link{datwrite}} (which wraps NMwriteData)
##'   \item ggplot/gtable objects or lists -> \code{\link{ggwrite}}
##' }
##'
##' For lists of objects, each element is written separately with appropriate naming.
##' File paths are constructed using the \code{fun.path} structure, which can organize
##' outputs by model, file name, or custom patterns.
##'
##' @return Invisibly returns the result from the called writer function.
##'
##' @seealso \code{\link{ggwrite}}, \code{\link{ftwrite}}, \code{\link{datwrite}}, 
##'   \code{\link{pathStruct}}
##'
##' @importFrom NMdata NMwriteData
##' @importFrom utils modifyList
##' @export
writer <- function(x,file,formats.ft,formats.gg,formats.data,##script=NULL,time,model=NULL,
                   fun.path,subdir=NULL,
                   ...){
  
  
  if(missing(formats.ft)||is.null(formats.ft)){
    formats.ft <- "png"
  }

  if(missing(formats.gg)||is.null(formats.gg)){
    formats.gg <- "png"
  }

  if(missing(formats.data)||is.null(formats.data)){
    formats.data <- "rds"
  }

  dots <- list(...)
  
  

  class_x <- "unknown"
  if("flextable" %in% class(x)) {
    class_x <- "flextable"
  }
  if( class_x == "unknown"&&is.data.frame(x)){
    class_x <- "data.frame"
  }
  if( class_x == "unknown"&&is.list(x) && !is.gg(x)){
    class_x <- "list"
  }
  if( class_x == "unknown"){
    stop("x is an unknown format",paste(class(x),collapse=", "))
  }
  
  if(class_x=="flextable") {
    
    dots <- dots[names(dots)%in%c(names(formals(ftwrite)))]
    args <- c(list(ft=x,
                   file = file,
                   formats=formats.ft,
                   fun.path=fun.path,
                   subdir=subdir
                   ),
              dots)
    res <- do.call(ftwrite,args)

  }

  if(class_x=="data.frame") {
    dots <- dots[unique(names(dots)%in%c(

      names(formals(NMwriteData)),
      names(formals(datwrite))
    ))]

    args <- c(list(x=x,
                   file = file,
                   formats=formats.data,
                   fun.path=fun.path
                   ),
              dots)
    res <- do.call(datwrite,args)
    
    
  }

  if(class_x=="list"){
    
    ## message("Calling ggwrite()")
    args.gg.def <- list(
      onefile=TRUE,
      canvas="standard"
    )
    
    dots <- dots[names(dots)%in%names(formals(ggwrite))]    
    args.gg <- modifyList(
      args.gg.def,
      dots)
    
    args.ggwrite <- modifyList(
      list(plot=x,
           file=file,
           use.names=TRUE,
           formats=formats.gg,
           fun.path=fun.path),
      args.gg
    )
    
    res <- try(do.call(ggwrite,args.ggwrite))
  }
  
  invisible(res)
}


### used to do this for data.frames
if(F){
  args.stamp <- dots[names(dots)%in%c("model")]
    if(!is.null(args.stamp$model) && is.list(args.stamp$model)) args.stamp$model <- args.stamp$model$lst
    
    args.fun.path <- dots[intersect(names(dots),c("name","model","subdir"))]
    #args.fun.path$file <- file

    dots <- dots[names(dots)%in%names(formals(NMwriteData))]
    
    args <- c(list(data=x,
                   file = do.call(fun.path,args.fun.path), 
                   formats=formats.data,
                   ##script=script,
                   genText=FALSE,
                   args.stamp=args.stamp
                   ),
              dots)
    if("time"%in%names(args)){
      args$args.stamp$time <- args$time
      args$time <- NULL
    }
    res <- do.call(NMwriteData,args)
    ##res <- do.call(datwrite,args )
}
