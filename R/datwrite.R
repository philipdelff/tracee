##' Write data to file(s) using NMwriteData
##'
##' This function writes data objects to files, with support for lists of data objects.
##' It wraps NMwriteData and provides additional functionality for file path construction
##' and metadata stamping.
##'
##' @param x A data frame or list of data frames to write
##' @param file Character string specifying the output file name
##' @param script Optional character string specifying the script name for stamping
##' @param time Optional time stamp to include in output
##' @param model Optional model object or path for organizing output files
##' @param formats Character vector of output formats (passed to NMwriteData)
##' @param path.format Character string or function defining the output path.
##'   Passed to \code{\link{pathFormat}}. Default (\code{NULL}) uses \code{file} as is.
##' @param subdir Optional subdirectory for organizing output files
##' @param quiet Logical. If \code{TRUE}, suppresses messages about ignored
##'   \code{model}/\code{script} arguments from \code{path.format}.
##' @param fun.path Deprecated. Use \code{path.format}.
##' @param ... Additional arguments passed to NMwriteData
##'
##' @return A list of results from NMwriteData calls
##' @export
datwrite <- function(x,file,script,time,model,formats,path.format=NULL,subdir=NULL,quiet=FALSE,fun.path=NULL,...){

  fun.path <- argPathFormat(path.format=path.format, fun.path=fun.path)

  if(!is.listnotplot(x)) x <- list(x)

  res <- lapply(x,
                FUN=datwriteOne,
                file=file,
                formats=formats,
                ## save=save,
                ## show=show,
                quiet=quiet,
                script=script,
                time=time,
                model=model,
                ## stamp.ft.style=stamp.ft.style,
                fun.path=fun.path,
                subdir=subdir,
                ...
                )

  res
}



##' Write a single data object to file
##'
##' Internal function that handles writing a single data frame to file using NMwriteData.
##' Constructs file paths, prepares stamping arguments, and calls NMwriteData with
##' appropriate parameters.
##'
##' @param x A data frame to write
##' @param file Character string specifying the output file name
##' @param formats Character vector of output formats
##' @param script Optional character string specifying the script name for stamping
##' @param time Optional time stamp to include in output
##' @param model Optional model object or path for organizing output files
##' @param fun.path Path function as returned by \code{\link{pathFormat}}, or \code{NULL}.
##' @param subdir Optional subdirectory for organizing output files
##' @param quiet Logical. If \code{TRUE}, suppresses messages about ignored
##'   \code{model}/\code{script} arguments from \code{fun.path}.
##' @param ... Additional arguments passed to NMwriteData
##'
##' @return Result from NMwriteData call
##' @keywords internal
datwriteOne <- function(x,file,formats,script,time,model,fun.path,subdir=NULL,quiet=FALSE,...){
  

  ## if(missing(stamp.ft.style)) stamp.ft.style <- NULL
  if(missing(fun.path)) fun.path <- NULL

  if(missing(script)) script <- NULL
  if(missing(time)) time <- NULL
  if(missing(model)) model <- NULL
  
  ## if(!is.null(fun.path)) file <- fun.path(name=file,model=model)

  args.stamp <- list(script=script,time=time,model=model)
  args.stamp <- args.stamp[!sapply(args.stamp,is.null)]
  
  if(!is.null(args.stamp$model) && is.list(args.stamp$model)){
    args.stamp$model <- args.stamp$model$lst
  }



  args.fun.path <- list(name=file,model=model)
  args.fun.path <- args.fun.path[!sapply(args.fun.path,is.null)]
  ## args.fun.path <- dots[intersect(names(dots),c("name","model","subdir"))]
  #args.fun.path$file <- file

  dots <- list(...)
  dots <- dots[names(dots)%in%names(formals(NMwriteData))]
  
  if(!is.null(fun.path)) file <- applyFunPath(fun.path,name=file,model=model,script=script,subdir=subdir,quiet=quiet)
  
  args <- c(list(data=x,
                 ## file = do.call(fun.path,args.fun.path), 
                 file = file,
                 #formats=formats,
                 ##script=script,
                 genText=FALSE,
                 args.stamp=args.stamp
                 ),
            dots)

  res <- do.call(NMwriteData,args)

  res
  
}
